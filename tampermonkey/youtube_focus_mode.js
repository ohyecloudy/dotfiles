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

    const xpath = "/html/body/ytd-app/div[1]/ytd-page-manager/ytd-watch-flexy/div[5]/div[2]";

    function removeByXPath(xpath) {
        const result = document.evaluate(xpath, document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null);
        const elem = result.singleNodeValue;
        if (elem) {
            console.log("? Removing element via XPath");
            elem.remove();
        } else {
            console.log("? Element not found, will retry");
        }
    }

    // 처음 한 번 시도하고, 이후 1초마다 재시도 (최대 10초)
    let attempts = 0;
    const maxAttempts = 10;
    const intervalId = setInterval(() => {
        removeByXPath(xpath);
        attempts++;
        if (attempts >= maxAttempts) {
            clearInterval(intervalId);
        }
    }, 1000);
})();
