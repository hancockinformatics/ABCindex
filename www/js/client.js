window.onbeforeunload = () => {
  // First check to see whether Shiny has disconnected
  if (document.getElementById("shiny-disconnected-overlay") === null) {
    // If Shiny is NOT disconnected, confirm exit
    return "If you navigate away, you will lose your results. Are you sure?";
  }
};
