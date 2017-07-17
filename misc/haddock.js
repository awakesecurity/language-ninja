// -----------------------------------------------------------------------------

let rspace = /\s\s+/g;
let rtrim = /^\s+|\s+$/g;

const spaced = (s) => (" " + s + " ").replace(rspace, " ");
const trim   = (s) => s.replace(rtrim, "");

const hasClass = (elem, value) => {
    const className = spaced(elem.className || "");
    return className.indexOf( " " + value + " " ) >= 0;
};

const addClass = (elem, value) => {
    const className = spaced(elem.className || "");
    if(className.indexOf( " " + value + " " ) < 0) {
        elem.className = trim(className + " " + value);
    }
};

const removeClass = (elem, value) => {
    let className = spaced(elem.className || "");
    className = className.replace(" " + value + " ", " ");
    elem.className = trim(className);
};

const toggleClass = (elem, valueOn, valueOff, bool) => {
    if(bool == null) { bool = !(hasClass(elem, valueOn)); }
    if(bool) {
        removeClass(elem, valueOff);
        addClass(elem, valueOn);
    } else {
        removeClass(elem, valueOn);
        addClass(elem, valueOff);
    }
    return bool;
};

const makeClassToggle = (valueOn, valueOff) => {
    return ((elem, bool) => {
        return toggleClass(elem, valueOn, valueOff, bool);
    });
};

const toggleShow = makeClassToggle("show", "hide");
const toggleCollapser = makeClassToggle("collapser", "expander");

const toggleSection = (id) => {
    const b = toggleShow(document.getElementById("section." + id));
    toggleCollapser(document.getElementById("control." + id), b);
    rememberCollapsed(id, b);
    return b;
};

let collapsed = {};

const rememberCollapsed = (id, b) => {
    if(b) {
        delete collapsed[id];
    } else {
        collapsed[id] = null;
    }

    let sections = [];
    for(let i in collapsed) {
        if(collapsed.hasOwnProperty(i))
            sections.push(i);
    }
    // cookie specific to this page; don't use setCookie which sets path=/
    document.cookie = "collapsed=" + escape(sections.join('+'));
};

const restoreCollapsed = () => {
    let cookie = getCookie("collapsed");
    if(!cookie) { return; }

    let ids = cookie.split('+');
    for(let i in ids) {
        if(document.getElementById("section." + ids[i])) {
            toggleSection(ids[i]);
        }
    }
};

const setCookie = (name, value) => {
    document.cookie = name + "=" + escape(value) + ";path=/;";
};

const clearCookie = (name) => {
    document.cookie = name + "=;path=/;expires=Thu, 01-Jan-1970 00:00:01 GMT;";
};

const getCookie = (name) => {
    let nameEQ = name + "=";
    let ca = document.cookie.split(';');
    for(let i = 0; i < ca.length; i++) {
        const c = ca[i];
        while(c.charAt(0) == ' ') { c = c.substring(1, c.length); }
        if(c.indexOf(nameEQ) == 0) {
            return unescape(c.substring(nameEQ.length, c.length));
        }
    }
    return null;
};

// 50 is not enough to search for map in the base libraries
let max_results = 75;

let shown_range = null;
let last_search = null;

const quick_search = () => { perform_search(false); };
const full_search  = () => { perform_search(true); };

const perform_search = (full) => {
    let text = document.getElementById("searchbox").value.toLowerCase();
    if(text == last_search && !full) { return; }
    last_search = text;

    let table = document.getElementById("indexlist");
    let status = document.getElementById("searchmsg");
    let children = table.firstChild.childNodes;

    // first figure out the first node with the prefix
    let first = bisect(-1);
    let last = (first == -1 ? -1 : bisect(1));

    if(first == -1) {
        table.className = "";
        status.innerHTML = "No results found, displaying all";
    } else if(first == 0 && last == children.length - 1) {
        table.className = "";
        status.innerHTML = "";
    } else if(last - first >= max_results && !full) {
        table.className = "";
        status.innerHTML = "More than " + max_results + ", press Search to display";
    } else {
        // decide what you need to clear/show
        if(shown_range) {
            setclass(shown_range[0], shown_range[1], "indexrow");
        }
        setclass(first, last, "indexshow");
        shown_range = [first, last];
        table.className = "indexsearch";
        status.innerHTML = "";
    }

    const setclass = (first, last, status) => {
        for(let i = first; i <= last; i++) {
            children[i].className = status;
        }
    };

    // do a binary search, treating 0 as ...
    // return either -1 (no 0's found) or location of most far match
    const bisect = (dir) => {
        let first = 0, finish = children.length - 1;
        let mid, success = false;

        while(finish - first > 3) {
            mid = Math.floor((finish + first) / 2);

            const i = checkitem(mid);

            if(i == 0) { i = dir; }

            if(i == -1) {
                finish = mid;
            } else {
                first = mid;
            }
        }

        let a = (dir == 1 ? first : finish);
        let b = (dir == 1 ? finish : first);

        for(let i = b; i != a - dir; i -= dir) {
            if(checkitem(i) == 0) { return i; }
        }

        return -1;
    };

    // from an index, decide what the result is
    // 0 = match, -1 is lower, 1 is higher
    const checkitem = (i) => {
        let s = getitem(i).toLowerCase().substr(0, text.length);
        if(s == text) {
            return 0;
        } else {
            return (s > text ? -1 : 1);
        }
    };

    // from an index, get its string
    // this abstracts over alternates
    const getitem = (i) => {
        while(i >= 0) {
            let s = children[i].firstChild.firstChild.data;
            if(s.indexOf(' ') == -1) { return s; }
            i--;
        }
        return ""; // should never be reached
    };
};

const setSynopsis = (filename) => {
    if(parent.window.synopsis && parent.window.synopsis.location) {
        if(parent.window.synopsis.location.replace) {
            // In Firefox this avoids adding the change to the history.
            parent.window.synopsis.location.replace(filename);
        } else {
            parent.window.synopsis.location = filename;
        }
    }
};

const addMenuItem = (html) => {
    let menu = document.getElementById("page-menu");
    if(menu) {
        let btn = menu.firstChild.cloneNode(false);
        btn.innerHTML = html;
        menu.appendChild(btn);
    }
};

const styles = () => {
    let a = undefined;
    let es = document.getElementsByTagName("link");
    let rs = [];
    for(let i = 0; a = es[i]; i++) {
        if(a.rel.indexOf("style") != -1 && a.title) { rs.push(a); }
    }
    return rs;
};

const addStyleMenu = () => {
    let as = styles();
    let a = undefined;
    let btns = "";
    for(let i = 0; a = as[i]; i++) {
        btns += ("<li><a href='#' onclick=\"setActiveStyleSheet('"
                 + a.title + "'); return false;\">"
                 + a.title + "</a></li>");
    }
    if(as.length > 1) {
        let h = "<div id='style-menu-holder'>"
            + "<a href='#' onclick='styleMenu(); return false;'>Style &#9662;</a>"
            + "<ul id='style-menu' class='hide'>" + btns + "</ul>"
            + "</div>";
        addMenuItem(h);
    }
};

const setActiveStyleSheet = (title) => {
    let a = undefined;
    let as = styles();
    let found = undefined;
    for(let i = 0; a = as[i]; i++) {
        a.disabled = true;
        // need to do this always, some browsers are edge triggered
        if(a.title == title) { found = a; }
    }
    if(found) {
        found.disabled = false;
        setCookie("haddock-style", title);
    } else {
        as[0].disabled = false;
        clearCookie("haddock-style");
    }
    styleMenu(false);
};

const resetStyle = () => {
    let s = getCookie("haddock-style");
    if(s) { setActiveStyleSheet(s); }
};

const styleMenu = (show) => {
    let m = document.getElementById('style-menu');
    if(m) { toggleShow(m, show); }
};

// -----------------------------------------------------------------------------

const importJavaScript = (src) => {
    let tag = document.createElement('script');
    tag.async = true;
    tag.type  = "text/javascript";
    tag.src   = src;
    const getByTag = ((tagName) => document.getElementsByTagName(tagName));
    (getByTag("head")[0] || getByTag("body")[0]).appendChild(tag);
};

// importJavaScript("https://code.jquery.com/jquery-3.2.1.min.js");
importJavaScript("jquery.js");

// -----------------------------------------------------------------------------

const parseSince = (contents) => {
    if(contents.slice(0, 7) === "Since: ") {
        const ver = contents.slice(7, contents.length);
        const versionRegex = /^(\d+[.])*\d+$/;
        if(versionRegex.test(ver)) { return ver; }
    }
    return null;
};

const addMetadata = () => {
    $("em").each(function() {
        const parsed = parseSince($(this).html());
        if(parsed === null) { return; }
        $(this).addClass("since");
        $(this).html(parsed);
        $(this).filter(":only-child").unwrap();
    });

    $("div.instances")
        .children("div.show")
        .children("table")
        .children("tbody")
        .children("tr")
        .each(function() {
            $(this).addClass("instance");
            $(this).children("td.src").addClass("instance-src");
            $(this).children("td.doc").addClass("instance-doc");
        });
};

// -----------------------------------------------------------------------------

const fixSince = () => {
    // Fix @since in module documentation
    if($("div#description em.since").is("*")) {
        let since = $("div#description > div.doc > em.since:last-child");
        const version = since.html();
        const titleText = "This module was added in version " + version;
        let versionTag = document.createElement("sup");
        $(versionTag).html(version);
        $(versionTag).addClass("module-version");
        $(versionTag).attr("title", titleText);
        $(since).remove();
        $("div#module-header > p.caption").append(versionTag);
    }

    // Fix @since in top-level declaration documentation
    $("div.top").each(function() {
        let since = $(this).children("div.doc").find("em.since");
        const version = since.html();
        since.remove();
        const titleText = "This declaration was added in version " + version;
        let versionTag = document.createElement("span");
        $(versionTag).html(version);
        $(versionTag).addClass("decl-version");
        $(versionTag).attr("title", titleText);
        $(this).addClass("versioned");
        $(this).attr("data-version", version);
        $(this).children("p.src").children(":last-child").before(versionTag);
    });

    // Fix @since in instance documentation
    $("div.instances").find("em.since").each(function() {
        const version = $(this).html();
        const titleText = "This instance was added in version " + version;
        let versionTag = document.createElement("span");
        $(versionTag).html(version);
        $(versionTag).attr("title", titleText);
        $(versionTag).addClass("instance-version");
        $(this).prev().append(versionTag);
        $(this).remove();
    });
    
    // Fix @since in constructor documentation
    $("div.constructors").find("em.since").each(function() {
        const version = $(this).html();
        const titleText = "This constructor was added in version " + version;
        let versionTag = document.createElement("span");
        $(versionTag).html(version);
        $(versionTag).attr("title", titleText);
        $(versionTag).addClass("constructor-version");
        $(this)
            .parent("td.doc")
            .prev("td.src")
            .prepend(versionTag);
        $(this).remove();
    });

    // Fix @since in method documentation
    $("div.methods").find("em.since").each(function() {
        const version = $(this).html();
        const titleText = "This method was added in version " + version;
        let versionTag = document.createElement("span");
        $(versionTag).html(version);
        $(versionTag).attr("title", titleText);
        $(versionTag).addClass("method-version");
        $(this)
            .parent("div.doc")
            .prev("p.src")
            .children("a.selflink")
            .prev("a.link")
            .before(versionTag);
        $(this).remove();
    });
};

// -----------------------------------------------------------------------------

const fixMisc = () => {
    const fixSelfLink = (tag) => {
        let col = document.createElement("td");
        $(col).addClass("instance-links");

        let source = $(tag).siblings("a.link:contains(Source)");
        $(source).detach();

        $(tag).parent("td.src").after(col);

        $(col).append(tag);
        $(col).append(source);
    };

    $("td.instance-src > a.selflink").each(function() { fixSelfLink(this); });

    document.documentElement.className = "reflow_" + (new Date()).getTime();
    document.documentElement.className = "";
};

// -----------------------------------------------------------------------------

const pageLoad = () => {
    addStyleMenu();
    resetStyle();
    restoreCollapsed();
    addMetadata();
    fixSince();
    fixMisc();
};

// -----------------------------------------------------------------------------
