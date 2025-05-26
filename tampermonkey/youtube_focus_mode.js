// ==UserScript==
// @name         youtube focus mode
// @namespace    http://ohyecloudy.com/
// @version      0.2
// @description  Don't waste your time on YouTube.
// @author       ohyecloudy@gmail.com
// @match        https://www.youtube.com/*
// @grant        none
// ==/UserScript==

(function() {
    'use strict';

    const HOMEPAGE_PATH = "/";
    const xpathHome = "/html/body/ytd-app/div[1]/ytd-page-manager/ytd-browse/ytd-two-column-browse-results-renderer/div[1]";
    const xpathGlobal = "/html/body/ytd-app/div[1]/ytd-page-manager/ytd-watch-flexy/div[5]/div[2]";

    function removeByXPath(xpath) {
        const result = document.evaluate(xpath, document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null);
        const elem = result.singleNodeValue;
        if (elem) {
            console.log("Removed element via XPath:", xpath);
            elem.remove();
        } else {
            console.log("Not found yet:", xpath);
        }
    }

    function applyRemovals() {
        const path = window.location.pathname;
        if (path === HOMEPAGE_PATH) {
            removeByXPath(xpathHome);
        } else {
            removeByXPath(xpathGlobal);
        }
    }

    // 시도 반복: 동적 로딩을 고려해 최대 10회까지 1초 간격 시도
    let attempts = 0;
    const maxAttempts = 10;
    const interval = setInterval(() => {
        applyRemovals();
        attempts++;
        if (attempts >= maxAttempts) {
            clearInterval(interval);
        }
    }, 1000);
})();
