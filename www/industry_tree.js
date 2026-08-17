// Custom Shiny.InputBinding backing industryTreeInput() (see app.R). A
// collapsible Aggregate -> 2-digit -> 3-digit tree dropdown with a search
// box, replacing the old flat indented-selectize Industry picker.
//
// R hands us a minimal skeleton -- a div.industry-tree (the bound element)
// holding a data-selected/data-placeholder pair and an embedded
// <script type="application/json"> with the nested tree. Everything
// visible (toggle button, search box, panel, rows) is built here in
// initialize()/renderTree(), and receiveMessage() re-runs the same
// renderTree() so there's exactly one place that builds this DOM instead
// of a server-rendered version that could drift from a JS-rebuilt one.
(function () {
  "use strict";

  function escapeHtml(str) {
    return String(str)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  // Plain-object stand-ins for Map/Set (and helpers for the DOM/NodeList
  // methods used below) -- this widget is the *only* thing under "Industry"
  // (industryTreeInput() emits no server-rendered fallback markup, just an
  // empty div + the JSON payload), so if this file throws anywhere during
  // initialize() the whole control silently stays blank with nothing else
  // on the page indicating why. Map/Set/NodeList.prototype.forEach/
  // Element.prototype.closest/the `:scope` combinator are all absent from
  // older engines that some collaborators' locked-down/managed machines may
  // still be on -- sticking to plain objects, arrays and manual loops here
  // costs nothing on modern browsers and removes that whole failure mode.
  function makeSet() { return Object.create(null); }
  function setAdd(set, v) { set[v] = true; }
  function setDelete(set, v) { delete set[v]; }
  function setHas(set, v) { return !!set[v]; }
  function cloneSet(set) {
    var out = Object.create(null);
    for (var k in set) if (Object.prototype.hasOwnProperty.call(set, k)) out[k] = true;
    return out;
  }
  function forEachOwn(obj, fn) {
    for (var k in obj) if (Object.prototype.hasOwnProperty.call(obj, k)) fn(obj[k], k);
  }
  function forEachNode(nodeList, fn) {
    for (var i = 0; i < nodeList.length; i++) fn(nodeList[i]);
  }
  // Walks up from `el` to the nearest ancestor (or itself) carrying
  // .industry-tree-row -- a hand-rolled Element.prototype.closest() for the
  // one selector this file ever needs it for.
  function closestRow(el) {
    while (el && el.nodeType === 1) {
      if (el.classList && el.classList.contains("industry-tree-row")) return el;
      el = el.parentNode;
    }
    return null;
  }

  function parseNodes(el) {
    var scriptEl = el.querySelector("script.industry-tree-data");
    if (!scriptEl) return [];
    try {
      return JSON.parse(scriptEl.textContent || "[]");
    } catch (e) {
      return [];
    }
  }

  // One DFS over the nested tree building value -> {label, parentValue}.
  // Depth/hasChildren live on the DOM (elementMap) instead, since the DOM
  // is what search/expand actually manipulate.
  function buildIndex(nodes, parentValue, index) {
    nodes.forEach(function (node) {
      index[node.value] = { label: node.label, parentValue: parentValue };
      if (node.children && node.children.length) {
        buildIndex(node.children, node.value, index);
      }
    });
    return index;
  }

  function nodeHtml(node, depth) {
    var hasChildren = !!(node.children && node.children.length);
    var rowClass = "industry-tree-row" + (hasChildren ? "" : " no-children");
    var html =
      '<li class="industry-tree-node" data-value="' + escapeHtml(node.value) + '">' +
      '<div class="' + rowClass + '" data-value="' + escapeHtml(node.value) + '" ' +
      'style="padding-left:' + (depth * 1.1) + 'rem" aria-expanded="false">' +
      '<span class="industry-tree-arrow" aria-hidden="true">&#9656;</span>' +
      '<span class="industry-tree-label" role="treeitem" tabindex="0">' + escapeHtml(node.label) + "</span>" +
      "</div>";
    if (hasChildren) {
      html +=
        '<ul class="industry-tree-children" role="group" hidden>' +
        node.children.map(function (child) { return nodeHtml(child, depth + 1); }).join("") +
        "</ul>";
    }
    html += "</li>";
    return html;
  }

  function getState(el) {
    return $(el).data("treeState");
  }

  // (Re)builds the toggle button + dropdown panel from `nodes`, replacing
  // whatever was there before (used both at init and on receiveMessage()
  // with a new tree_data). The embedded <script> tag from R is left alone
  // -- it's only ever read once per renderTree() call, not re-derived from.
  function renderTree(el, nodes, initialValue) {
    $(el).find(".industry-tree-toggle, .industry-tree-panel").remove();

    var index = buildIndex(nodes, null, {});
    var listHtml = nodes.map(function (node) { return nodeHtml(node, 0); }).join("");
    var html =
      '<button type="button" class="industry-tree-toggle" aria-haspopup="true" aria-expanded="false">' +
      '<span class="industry-tree-toggle-label"></span></button>' +
      '<div class="industry-tree-panel" hidden>' +
      '<input type="text" class="industry-tree-search" placeholder="Search industries...">' +
      '<ul class="industry-tree-list" role="tree">' + listHtml + "</ul>" +
      "</div>";
    $(el).append(html);

    // Plain object, not Map -- see the comment above makeSet(). Keyed by
    // industry name; li.querySelector(".industry-tree-row") (no `:scope >`)
    // still reliably returns *this* li's own direct row/children-list, not
    // some deeper-nested descendant's -- the direct child is always first
    // in document order, so querySelector's depth-first search finds it
    // before ever descending into it.
    var elementMap = Object.create(null);
    forEachNode(el.querySelectorAll(".industry-tree-node"), function (li) {
      elementMap[li.getAttribute("data-value")] = {
        li: li,
        row: li.querySelector(".industry-tree-row"),
        childList: li.querySelector(".industry-tree-children")
      };
    });

    var toggle = el.querySelector(".industry-tree-toggle");
    var search = el.querySelector(".industry-tree-search");
    toggle.setAttribute("aria-controls", el.id ? el.id + "-panel" : "");

    var state = {
      index: index,
      elementMap: elementMap,
      expanded: makeSet(),
      expandSnapshot: null,
      searchActive: false,
      value: "",
      toggle: toggle,
      toggleLabel: el.querySelector(".industry-tree-toggle-label"),
      panel: el.querySelector(".industry-tree-panel"),
      search: search,
      placeholder: el.getAttribute("data-placeholder") || ""
    };
    $(el).data("treeState", state);

    search.value = "";
    setValue(el, initialValue);
  }

  function setValue(el, value) {
    var state = getState(el);
    if (!state) return;
    value = value || "";
    state.value = value;
    el.setAttribute("data-selected", value);

    var info = state.index[value];
    if (info) {
      state.toggleLabel.textContent = info.label;
      state.toggleLabel.classList.remove("industry-tree-empty");
    } else {
      state.toggleLabel.textContent = state.placeholder;
      // Namespaced rather than plain "placeholder" -- that class name
      // collides with Bootstrap 5's own built-in .placeholder loading-
      // skeleton utility (background-color: currentColor; opacity: 0.5),
      // which painted a solid grey box over the text instead of just
      // tinting it.
      state.toggleLabel.classList.add("industry-tree-empty");
    }

    forEachOwn(state.elementMap, function (refs, nodeValue) {
      refs.row.classList.toggle("selected", nodeValue === value);
    });
  }

  function setExpanded(el, value, expand) {
    var state = getState(el);
    var refs = state.elementMap[value];
    if (!refs || !refs.childList) return;
    if (expand) setAdd(state.expanded, value); else setDelete(state.expanded, value);
    refs.childList.hidden = !expand;
    refs.row.setAttribute("aria-expanded", expand ? "true" : "false");
    refs.row.classList.toggle("expanded", expand);
  }

  function expandAncestors(el, value) {
    var state = getState(el);
    var info = state.index[value];
    var parent = info ? info.parentValue : null;
    while (parent) {
      setExpanded(el, parent, true);
      var parentInfo = state.index[parent];
      parent = parentInfo ? parentInfo.parentValue : null;
    }
  }

  function openPanel(el) {
    var state = getState(el);
    state.panel.hidden = false;
    state.toggle.setAttribute("aria-expanded", "true");
    if (state.value) expandAncestors(el, state.value);
    window.setTimeout(function () { state.search.focus(); }, 0);
  }

  function closePanel(el) {
    var state = getState(el);
    if (!state) return;
    state.panel.hidden = true;
    state.toggle.setAttribute("aria-expanded", "false");
  }

  // Search filters to label-substring matches plus every ancestor of a
  // match (so the path down to it stays visible), hiding everything else.
  // On the first keystroke of a search "session" the current expand state
  // is snapshotted so clearing the box restores exactly that instead of
  // always collapsing back to root-only.
  function applySearch(el, rawQuery) {
    var state = getState(el);
    var query = rawQuery.trim().toLowerCase();

    if (query === "") {
      if (state.searchActive) {
        state.expanded = state.expandSnapshot || makeSet();
        state.expandSnapshot = null;
        state.searchActive = false;
        forEachOwn(state.elementMap, function (refs, value) {
          refs.li.hidden = false;
          if (refs.childList) {
            var expand = setHas(state.expanded, value);
            refs.childList.hidden = !expand;
            refs.row.setAttribute("aria-expanded", expand ? "true" : "false");
            refs.row.classList.toggle("expanded", expand);
          }
        });
      }
      return;
    }

    if (!state.searchActive) {
      state.expandSnapshot = cloneSet(state.expanded);
      state.searchActive = true;
    }

    var visible = makeSet();
    Object.keys(state.index).forEach(function (value) {
      var info = state.index[value];
      if (info.label.toLowerCase().indexOf(query) === -1) return;
      setAdd(visible, value);
      var parent = info.parentValue;
      while (parent) {
        setAdd(visible, parent);
        var parentInfo = state.index[parent];
        parent = parentInfo ? parentInfo.parentValue : null;
      }
    });

    forEachOwn(state.elementMap, function (refs, value) {
      var show = setHas(visible, value);
      refs.li.hidden = !show;
      if (refs.childList) {
        refs.childList.hidden = !show;
        refs.row.setAttribute("aria-expanded", show ? "true" : "false");
      }
    });
  }

  var industryTreeBinding = new Shiny.InputBinding();

  $.extend(industryTreeBinding, {
    find: function (scope) {
      return $(scope).find(".industry-tree");
    },

    initialize: function (el) {
      renderTree(el, parseNodes(el), el.getAttribute("data-selected") || "");
    },

    getValue: function (el) {
      var state = getState(el);
      return (state && state.value) || "";
    },

    setValue: function (el, value) {
      setValue(el, value);
    },

    subscribe: function (el, callback) {
      $(el).on("click.industryTree", ".industry-tree-toggle", function (e) {
        e.preventDefault();
        var state = getState(el);
        if (state.panel.hidden) openPanel(el); else closePanel(el);
      });

      $(el).on("click.industryTree", ".industry-tree-arrow", function (e) {
        e.stopPropagation();
        var row = closestRow(e.currentTarget);
        var value = row.getAttribute("data-value");
        var state = getState(el);
        setExpanded(el, value, !setHas(state.expanded, value));
      });

      function selectRow(row) {
        var value = row.getAttribute("data-value");
        setValue(el, value);
        closePanel(el);
        getState(el).toggle.focus();
        callback(true);
      }

      $(el).on("click.industryTree", ".industry-tree-label", function (e) {
        selectRow(closestRow(e.currentTarget));
      });

      $(el).on("keydown.industryTree", ".industry-tree-label", function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          selectRow(closestRow(e.currentTarget));
        }
      });

      $(el).on("input.industryTree", ".industry-tree-search", function (e) {
        applySearch(el, e.target.value);
      });

      $(el).on("keydown.industryTree", function (e) {
        if (e.key === "Escape") {
          closePanel(el);
          getState(el).toggle.focus();
        }
      });

      // Bare (non-delegated) change handler -- this is what makes
      // $(el).trigger("change") in receiveMessage() actually notify Shiny,
      // the same idiom every built-in input binding uses.
      $(el).on("change.industryTree", function () {
        callback(true);
      });

      $(document).on("click.industryTree-" + el.id, function (e) {
        if (!el.contains(e.target)) closePanel(el);
      });
    },

    unsubscribe: function (el) {
      $(el).off(".industryTree");
      $(document).off(".industryTree-" + el.id);
    },

    receiveMessage: function (el, data) {
      var state = getState(el);
      var priorValue = state ? state.value : (el.getAttribute("data-selected") || "");
      if (data.hasOwnProperty("tree_data")) {
        renderTree(el, data.tree_data, data.hasOwnProperty("selected") ? data.selected : priorValue);
      } else if (data.hasOwnProperty("selected")) {
        setValue(el, data.selected);
      }
      $(el).trigger("change");
    }
  });

  Shiny.inputBindings.register(industryTreeBinding, "productivitydashboard.industryTree");
})();
