// Small generic Shiny custom-message handlers shared across tabs -- kept
// separate from industry_tree.js, which is scoped to just the tree
// dropdown's own Shiny.InputBinding.
(function () {
  "use strict";

  // Toggles the native `disabled` attribute on any element by id (e.g. the
  // Compare/Data "Add series" button, disabled until both Industry and
  // Geography are picked -- see tab_module_server()'s observe() on
  // input$pair_industry/input$pair_geography). A disabled <button> simply
  // stops dispatching click events in the browser, so this alone is
  // enough to block "Add series" -- no server-side guard needed beyond
  // what req() already does.
  Shiny.addCustomMessageHandler("toggleDisabled", function (msg) {
    var el = document.getElementById(msg.id);
    if (!el) return;
    el.disabled = !!msg.disabled;
  });

  // Client-side PNG export for the Trends/Rankings/Compare tabs' Plotly
  // charts, wired to the "Download chart as PNG" item in each tab's
  // Download dropdown (see download_menu_ui() in app.R). Every
  // plotlyOutput() renders straight into a <div id="<ns>-chart"> that
  // plotly.js turns into the actual graph div, so there's nothing to
  // render server-side here -- this just hands that already-rendered div
  // back to Plotly's own PNG exporter. Exposed on window (rather than
  // kept inside this IIFE's closure) since it's invoked from a plain
  // inline onclick attribute, which only ever runs in global scope.
  window.downloadChartPng = function (targetId) {
    var gd = document.getElementById(targetId);
    // Guards a chart that hasn't rendered yet (e.g. no data for the
    // current selection) -- silently does nothing rather than throwing,
    // same "nothing to export" outcome a disabled button would give.
    if (!gd || typeof Plotly === "undefined" || !gd.data) return;

    var titleText = (gd.layout && gd.layout.title && gd.layout.title.text) || "chart";
    // Chart titles can carry embedded HTML (the Trends tab's
    // "<br><sup>...</sup>" subtitle) -- strip tags down to plain text
    // before turning it into a filename.
    var plain = titleText.replace(/<[^>]*>/g, " ").replace(/\s+/g, " ").trim();
    var slug = plain.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "");
    // Local calendar date, not UTC -- toISOString() reports the UTC date,
    // which can land a day off from the CSV downloads' filenames (built
    // server-side from Sys.Date(), the local date) for anyone west of UTC
    // in the evening.
    var now = new Date();
    var pad2 = function (n) { return n < 10 ? "0" + n : "" + n; };
    var today = now.getFullYear() + pad2(now.getMonth() + 1) + pad2(now.getDate());

    Plotly.downloadImage(gd, {
      format: "png",
      filename: "productivity_" + (slug || "chart") + "_" + today
    });
  };

  // Hides the #app-splash "Starting the application" / "Loading data"
  // overlay (see ui()/page_fillable() in app.R) once the app has actually
  // finished its first real work -- shiny:idle (not shiny:sessioninitialized,
  // which fires as soon as the server's init message arrives, before the
  // default view has actually rendered) is the first point the initial
  // chart is genuinely done computing. .one(), not .on() -- only the very
  // first idle matters; every later busy/idle cycle is a normal chart
  // update, handled by useBusyIndicators() instead (see app.R), not this
  // splash. The 8s timeout is a failsafe only, in case that first
  // busy/idle cycle is ever skipped entirely.
  $(document).one("shiny:idle", hideSplash);
  setTimeout(hideSplash, 8000);
  function hideSplash() {
    var el = document.getElementById("app-splash");
    if (!el) return;
    el.classList.add("app-splash-hidden");
    setTimeout(function () { el.remove(); }, 400);
  }

  // Turns the #shiny-disconnected-overlay Shiny itself injects/removes on
  // disconnect (styled in csls-shiny-theme.css) into a working "Connection
  // lost -- click to reload" retry action -- a plain document-level
  // delegated listener, not one bound to the overlay element directly,
  // since that element doesn't exist yet at page load and Shiny gives no
  // JS hook to attach to the moment it's created. A no-op the rest of the
  // time (nothing on the page ever carries this id while connected).
  document.addEventListener("click", function (e) {
    if (e.target && e.target.id === "shiny-disconnected-overlay") {
      window.location.reload();
    }
  });
})();
