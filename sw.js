/// <reference lib="webworker" />

const CACHE_VERSION = "phylosaur-shell-v188";
const CORE_ASSETS = [
  "./",
  "./index.html",
  "./about.html",
  "./offline.html",
  "./manifest.webmanifest",
  "./phylosaur_db.json",
  "./phylosaur_media_overrides.json?v=24",
  "./phylosaur_paleodata.json",
  "./style.css?v=188",
  "./js/config.js?v=188",
  "./js/state.js?v=188",
  "./js/i18n.js?v=188",
  "./js/api.js?v=188",
  "./js/autocomplete.js?v=188",
  "./js/db.js?v=188",
  "./js/auth.js?v=188",
  "./js/ui.js?v=188",
  "./js/screens.js?v=188",
  "./js/tree.js?v=188",
  "./js/game.js?v=188",
  "./js/main.js?v=188"
];

// Decorative/install icons must not invalidate an otherwise complete shell.
const OPTIONAL_ASSETS = [
  "./pwa-icon-192.png",
  "./pwa-icon-512.png",
  "./apple-touch-icon.png",
  "./pwa-icon.svg"
];

async function cacheAvailableCoreAssets() {
  const cache = await caches.open(CACHE_VERSION);
  const cacheAsset = async (asset) => {
    const request = new Request(asset, { cache: "reload" });
    const response = await fetch(request);
    if (!response.ok) throw new Error(`Required asset unavailable: ${asset} (${response.status})`);
    await cache.put(request, response);
  };

  // Wait for every writer before deleting a failed cache: no late put may
  // recreate a partial release after installation has been rejected.
  const results = await Promise.allSettled(CORE_ASSETS.map(cacheAsset));
  const failure = results.find((result) => result.status === "rejected");
  if (failure) {
    await caches.delete(CACHE_VERSION).catch(() => false);
    throw failure.reason;
  }
  await Promise.allSettled(OPTIONAL_ASSETS.map(cacheAsset));
}

self.addEventListener("install", (event) => {
  event.waitUntil((async () => {
    await cacheAvailableCoreAssets();
    await self.skipWaiting();
  })());
});

self.addEventListener("activate", (event) => {
  event.waitUntil((async () => {
    const cacheNames = await caches.keys();
    await Promise.all(cacheNames
      .filter((name) => /^phylosaur-shell-v\d+$/.test(name) && name !== CACHE_VERSION)
      .map((name) => caches.delete(name).catch(() => false)));
    await self.clients.claim();
  })());
});

async function readCachedAsset(request) {
  try {
    const cache = await caches.open(CACHE_VERSION);
    return await cache.match(request);
  } catch (_error) {
    return null;
  }
}

function fetchAndCache(request, event) {
  const networkResponse = fetch(request);
  // Register synchronously during fetch dispatch; storage work does not delay
  // the network response or turn quota/cache failures into network failures.
  event.waitUntil(networkResponse.then(async (response) => {
    if (!response.ok) return;
    const copy = response.clone();
    const cache = await caches.open(CACHE_VERSION);
    await cache.put(request, copy);
  }).catch(() => null));
  return networkResponse;
}

async function onlineNavigation(request, event) {
  try {
    return await fetchAndCache(request, event);
  } catch (_error) {
    return (await readCachedAsset("./offline.html")) || new Response(
      "Phylosaur is offline. Reconnect and try again.",
      { status: 503, headers: { "Content-Type": "text/plain; charset=utf-8" } }
    );
  }
}

async function cachedStaticAsset(request, event) {
  const networkRequest = fetchAndCache(request, event).catch(() => null);
  const cached = await readCachedAsset(request);

  if (cached) return cached;
  return (await networkRequest) || new Response("Asset unavailable offline.", {
    status: 504,
    headers: { "Content-Type": "text/plain; charset=utf-8" }
  });
}

async function freshStaticAsset(request, event) {
  try {
    return await fetchAndCache(request, event);
  } catch (_error) {
    return (await readCachedAsset(request)) || new Response("Asset unavailable offline.", {
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
    event.respondWith(onlineNavigation(request, event));
    return;
  }

  const isApplicationCode = ["style", "script"].includes(request.destination)
    || /\.(?:css|js)$/i.test(url.pathname);

  if (isApplicationCode) {
    event.respondWith(freshStaticAsset(request, event));
    return;
  }

  const isStaticAsset = ["image", "font"].includes(request.destination)
    || /\.(?:png|jpe?g|svg|webp|woff2?|json)$/i.test(url.pathname);

  if (isStaticAsset) event.respondWith(cachedStaticAsset(request, event));
});