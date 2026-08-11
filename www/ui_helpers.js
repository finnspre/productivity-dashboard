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
})();
