import { writable, readable } from 'svelte/store';
import { parse as parseParam } from 'regexparam';


// Routing

const hist = readable(window.location.href, function start(set) {
    console.log("[History] INIT")

    const originalPushState = history.pushState;
    const originalReplaceState = history.replaceState;

    const updateHref = () => set(window.location.href);

    history.pushState = function () {
        originalPushState.apply(this, arguments)
        updateHref()
    }

    history.replaceState = function () {
        originalReplaceState.apply(this, arguments)
        updateHref()
    }
    window.addEventListener('popstate', updateHref);
    window.addEventListener('hashchange', updateHref);

    if (window.location.hash == '') {
        history.replaceState(null, '', window.location.href + '#/')
    }

    return function stop() {
        console.log("[History] DEINIT")
        history.pushState = originalPushState;
        history.replaceState = originalReplaceState;
        window.removeEventListener('popstate', updateHref)
        window.removeEventListener('hashchange', updateHref)
    };
});


const exec = (path, result) => {
    let i = 0, out = {};
    let matches = result.pattern.exec(path);

    while (i < result.keys.length) {
        out[result.keys[i]] = matches[++i] || null;
    }
    return out;
}

const find_route = (routes, href) => {
    return routes.find(({ match }) => match.pattern.test(href));
}

export const router = (routes) => {
    let m = [];

    for (let route of routes) {
        const match = parseParam(route.name);
        m.push({ ...route, match });
    }

    const route404 = find_route(m, '#/404');
    let route = writable(undefined);

    hist.subscribe((href) => {
        let hash = '/' + (new URL(href).hash);

        let result = find_route(m, hash);
        if (result !== undefined) {
            console.log("[ROUTE]", result.name);
            route.set({
                component: result.component,
                params: exec(hash, result.match)
            });
        } else {
            console.log("[ROUTE404]", route404);
            route.set(route404);// or undefined
        }
    });

    return route;
};

// Toast
export let toast = writable({message: null, kind: 'error'});