// ==UserScript==
// @name         youtube focus mode
// @namespace    http://ohyecloudy.com/
// @version      0.1
// @description  Don't waste your time on YouTube.
// @author       ohyecloudy@gmail.com
// @match        https://www.youtube.com/*
// @require      https://code.jquery.com/jquery-2.2.4.min.js#sha256=BbhdlvQf/xTY9gja0Dq3HiwQF8LaCRTXxZKRutelT44=
// @require      https://gist.github.com/raw/2625891/waitForKeyElements.js#sha256=fe967293ad8e533dd8ad61e20461c1fe05d369eed31e6d3e73552c2231b528da
// ==/UserScript==

(function() {
    'use strict';
    // recommended videos
    waitForKeyElements(
        "#secondary",
        removeElement
    );
})();

function removeElement(elem) {
    elem.remove();
}
