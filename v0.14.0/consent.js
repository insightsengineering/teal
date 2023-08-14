(function ($) {
    "use strict";
    $.fn.cookieWall = function (options) {
        const params = $.extend({
            id: '',
            cookie: {
                name: 'nest-documentation',
                days: 15,
                path: '/'
            },
            tag: {
                cookiePrefix: '',
                cookieDomain: '',
                cookieExpires: '',
                cookieUpdate: ''
            }
        }, options);
        const tag_params = {}
        for (const property in params.tag) {
            if (params.tag[property] != '') {
                tag_params[property.replace(/([A-Z])/g, "-$1").toLowerCase()] = params.tag[property];
            }
        }
        const tag = '<script async src="https://www.googletagmanager.com/gtag/js?id=' + params.id + '"></script>' +
            '<script>' +
            'window.dataLayer = window.dataLayer || [];' +
            'function gtag(){dataLayer.push(arguments);}' +
            'gtag(\'js\', new Date());' +
            'gtag(\'config\', \'' + params.id + '\', ' + JSON.stringify(tag_params).replace(/"/g, '\'') + ');' +
            '</script>';

        const cookieNotification = `
        <div class="cookie-consent">
            <span>This site uses cookies to enhance user experience. Please see the <a href="cookie_policy.txt"
                class="ml-1 text-decoration-none">privacy policy</a> for more info.</span>
            <div class="mt-2 d-flex align-items-center justify-content-center g-2">
                <button class="cookie-button" id="cookie_accept">Accept</button>
                <button class="cookie-button" id="cookie_deny">Dismiss</button>
            </div>
        </div>`;

        function init() {
            if (params.id != '') {
                let c = getCookie();
                if (c == null || (c != 0 && c != 1)) {
                    displayCookieNotification();
                } else if (c == 1) {
                    addTag();
                }
            } else {
                console.log('No ID defined in the cookieWall params.');
            }
        }

        function displayCookieNotification() {
            $('body').prepend(cookieNotification);
            $('body').on('mousedown', '.cookie-consent .cookie-button', setChoice);
        }

        function removeCookieNotification() {
            $('body .cookie-consent').remove();
        }

        function setChoice(e) {
            e.preventDefault();
            if (this.id == 'cookie_accept') {
                setCookie(1);
                addTag();
            } else {
                setCookie(0);
            }
            removeCookieNotification();
        }

        function addTag() {
            $('body').append(tag);
        }

        function getCookie() {
            let t = document.cookie.split('; ');
            let f = t.find(row => row.startsWith(params.cookie.name + '='));
            if (typeof f != 'undefined') {
                return f.split('=')[1];
            }
            return null;
        }

        function setCookie(value) {
            let a = params.cookie.days * 86400;
            document.cookie = params.cookie.name + '=' + value + ';max-age=' + a + ';path=' + params.cookie.path + ';SameSite=None;Secure';
        }
        init();
        return this;
    };
})(jQuery);
