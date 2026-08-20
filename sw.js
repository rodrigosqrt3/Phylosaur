/// <reference lib="webworker" />

const CACHE_VERSION = "phylosaur-shell-v5";
const CORE_ASSETS = [
  "./",
  "./index.html",
  "./about.html",
  "./offline.html",
  "./manifest.webmanifest",
  "./style.css",
  "./js/config.js",
  "./js/state.js",
  "./js/api.js",
  "./js/autocomplete.js",
  "./js/db.js",
  "./js/auth.js",
  "./js/ui.js",
  "./js/screens.js",
  "./js/tree.js",
  "./js/game.js",
  "./js/main.js",
  "./pwa-icon-192.png",
  "./pwa-icon-512.png",
  "./apple-touch-icon.png",
  "./pwa-icon.svg"
];

async function cacheAvailableCoreAssets() {
  const cache = await caches.open(CACHE_VERSION);

  await Promise.allSettled(CORE_ASSETS.map(async (asset) => {
    const request = new Request(asset, { cache: "reload" });
    const response = await fetch(request);
    if (response.ok) await cache.put(request, response);
  }));
}

self.addEventListener("install", (event) => {
  event.waitUntil(cacheAvailableCoreAssets());
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil((async () => {
    const cacheNames = await caches.keys();
    await Promise.all(cacheNames
      .filter((name) => name.startsWith("phylosaur-") && name !== CACHE_VERSION)
      .map((name) => caches.delete(name)));
    await self.clients.claim();
  })());
});

async function onlineNavigation(request) {
  try {
    const response = await fetch(request);
    if (response.ok) {
      const cache = await caches.open(CACHE_VERSION);
      await cache.put(request, response.clone());
    }
    return response;
  } catch (_error) {
    return (await caches.match("./offline.html")) || new Response(
      "Phylosaur is offline. Reconnect and try again.",
      { status: 503, headers: { "Content-Type": "text/plain; charset=utf-8" } }
    );
  }
}

async function cachedStaticAsset(request) {
  const cached = await caches.match(request);
  const networkRequest = fetch(request).then(async (response) => {
    if (response.ok) {
      const cache = await caches.open(CACHE_VERSION);
      await cache.put(request, response.clone());
    }
    return response;
  });

  return cached || networkRequest;
}

async function freshStaticAsset(request) {
  try {
    const response = await fetch(request);
    if (response.ok) {
      const cache = await caches.open(CACHE_VERSION);
      await cache.put(request, response.clone());
    }
    return response;
  } catch (_error) {
    return (await caches.match(request)) || new Response("Asset unavailable offline.", {
      status: 504,
      headers: { "Content-Type": "text/plain; charset=utf-8" }
    });
  }
}

self.addEventListener("fetch", (event) => {
  const { request } = event;
  const url = new URL(request.url);

  if (request.method !== "GET" || url.origin !== self.location.origin) return;

  if (request.mode === "navigate") {
    event.respondWith(onlineNavigation(request));
    return;
  }

  const isApplicationCode = ["style", "script"].includes(request.destination)
    || /\.(?:css|js)$/i.test(url.pathname);

  if (isApplicationCode) {
    event.respondWith(freshStaticAsset(request));
    return;
  }

  const isStaticAsset = ["image", "font"].includes(request.destination)
    || /\.(?:png|jpe?g|svg|webp|woff2?|json)$/i.test(url.pathname);

  if (isStaticAsset) event.respondWith(cachedStaticAsset(request));
});