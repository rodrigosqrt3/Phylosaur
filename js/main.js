const sb = window.supabase?.createClient(SUPABASE_URL, SUPABASE_ANON_KEY) || null;

let lastTreeViewportWidth = window.innerWidth;
let treeResizeTimer = null;
let isRestoringAppRoute = false;
let appRouteRestoreGeneration = 0;
let appRoutingReady = false;

const APP_ROUTE_DIFFICULTIES = new Set([
  'muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil'
]);

// Screens slide in from the right when you go deeper and from the left when you go back (CSS,
// keyed on <html data-nav-dir>). Going to the home screen counts as going back.
let lastRouteDepth = Number(window.history.state?.phylosaurDepth || 0);

function trackNavigation(direction, depth = lastRouteDepth) {
  lastRouteDepth = depth;
  const root = typeof document !== 'undefined' ? document.documentElement : null;
  if (root?.dataset) root.dataset.navDir = direction;
}

// Addresses name levels by number (#/game/daily/3). The internal level keys ('muito_facil', ...)
// stay what the server, the saved progress and the sessions use; only the address changes.
// Older links with the key in the address keep working and are rewritten to the numeric form.
const PHYLOSAUR_LEVEL_ROUTE_SEGMENTS = Object.freeze({
  muito_facil: '1', facil: '2', normal: '3', dificil: '4', muito_dificil: '5'
});

// A custom practice pool ('custom:facil,normal:Theropoda') is written with level numbers too
// ('custom:2,3:Theropoda'); its internal key is what the server receives.
function levelToRouteSegment(difficulty) {
  if (PHYLOSAUR_LEVEL_ROUTE_SEGMENTS[difficulty]) return PHYLOSAUR_LEVEL_ROUTE_SEGMENTS[difficulty];
  const pool = /^custom:([a-z_,]+):(.+)$/.exec(String(difficulty));
  if (!pool) return difficulty;
  const numbers = pool[1].split(',').map(level => PHYLOSAUR_LEVEL_ROUTE_SEGMENTS[level] || level);
  return `custom:${numbers.join(',')}:${pool[2]}`;
}

function routeSegmentToLevel(segment) {
  const levelOf = piece => Object.keys(PHYLOSAUR_LEVEL_ROUTE_SEGMENTS)
    .find(key => PHYLOSAUR_LEVEL_ROUTE_SEGMENTS[key] === piece) || piece;
  const pool = /^custom:([a-z_0-9,]+):(.+)$/.exec(String(segment));
  if (pool) return `custom:${pool[1].split(',').map(levelOf).join(',')}:${pool[2]}`;
  return levelOf(segment);
}

function normalizeAppRoute(route) {
  const value = String(route || '/').trim();
  if (!value || value === '#' || value === '#/') return '/';
  const withoutHash = value.replace(/^#/, '');
  return withoutHash.startsWith('/') ? withoutHash : `/${withoutHash}`;
}

function getCurrentAppRoute() {
  if (!window.location.hash.startsWith('#/')) return '/';
  return normalizeAppRoute(window.location.hash);
}

function buildAppRouteUrl(route) {
  const normalized = normalizeAppRoute(route);
  const base = `${window.location.pathname}${window.location.search}`;
  return normalized === '/' ? base : `${base}#${normalized}`;
}

function setAppRoute(route, { replace = false } = {}) {
  if (isRestoringAppRoute) return;
  appRouteRestoreGeneration++;
  // pushState does not dispatch popstate: direct screen navigation also needs
  // to dismiss an open result image and any dialogs covering it, top first.
  if (document.getElementById?.('image-lightbox')) {
    closeTransientRouteOverlays({ closeMuseum: false });
  }

  const normalized = normalizeAppRoute(route);
  const currentRoute = getCurrentAppRoute();
  const currentDepth = Number(window.history.state?.phylosaurDepth || 0);
  const alreadyTracked = window.history.state?.phylosaurRoute === normalized;

  if (currentRoute === normalized && alreadyTracked) return;

  const shouldReplace = replace || currentRoute === normalized;
  const nextState = {
    ...(window.history.state || {}),
    phylosaurRoute: normalized,
    phylosaurDepth: shouldReplace ? currentDepth : currentDepth + 1
  };
  if (typeof trackNavigation === 'function') {
    trackNavigation(normalized === '/' ? 'back' : 'forward', nextState.phylosaurDepth);
  }

  window.history[shouldReplace ? 'replaceState' : 'pushState'](
    nextState,
    document.title,
    buildAppRouteUrl(normalized)
  );
}

function closeTransientRouteOverlays({ closeMuseum = true } = {}) {
  let removedUnmanagedOverlay = false;
  Array.from(document.querySelectorAll('.modal-overlay, .tutorial-overlay, .image-lightbox')).reverse().forEach(element => {
    if (typeof element.dismissAppOverlay === 'function') {
      element.dismissAppOverlay({ restoreFocus: false });
    } else {
      element.remove();
      removedUnmanagedOverlay = true;
    }
  });
  if (removedUnmanagedOverlay) document.body.style.overflow = '';
  if (closeMuseum && typeof closeMuseumEntry === 'function') closeMuseumEntry();
}

async function renderFallbackRoute(route) {
    const normalized = normalizeAppRoute(route);
    if (normalized === '/tutorial') return startTutorialGame();
    if (normalized === '/practice') return showPracticeMode();
  if (normalized === '/friends') return showFriendChallenges();
  return showDifficultySelection();
}

function navigateBackOrHome(fallbackRoute = '/') {
  const depth = Number(window.history.state?.phylosaurDepth || 0);
  if (depth > 0) {
    window.history.back();
    return;
  }
  renderFallbackRoute(fallbackRoute);
}

function navigateToAppRoute(route = '/') {
  const normalized = normalizeAppRoute(route);
  if (typeof trackNavigation === 'function') trackNavigation(normalized === '/' ? 'back' : 'forward', 0);
  window.history.replaceState({
    ...(window.history.state || {}),
    phylosaurRoute: normalized,
    phylosaurDepth: 0
  }, document.title, buildAppRouteUrl(normalized));

  if (appRoutingReady) return restoreAppRoute();
  return renderFallbackRoute(normalized);
}

async function restoreAppRoute() {
  let route = getCurrentAppRoute();
  const requestedRoute = route;
  const generation = ++appRouteRestoreGeneration;
  const ownerId = currentUserId;
  const isCurrentRequest = () => generation === appRouteRestoreGeneration
    && getCurrentAppRoute() === requestedRoute && currentUserId === ownerId;
  const renderRoute = render => {
    isRestoringAppRoute = true;
    try {
      return render();
    } finally {
      isRestoringAppRoute = false;
    }
  };
  const parts = route.split('/').filter(Boolean);

  const museumVisible = Boolean(document.querySelector('#app-content .museum-grid'));
  if (route === '/museum' && museumVisible) {
    closeTransientRouteOverlays({ closeMuseum: false });
    await closeMuseumEntry({ animate: true, restorePosition: true });
    if (!isCurrentRequest()) return getCurrentAppRoute();
  } else {
    closeTransientRouteOverlays();
  }
  if (route === '/') {
    await renderRoute(() => showDifficultySelection());
  } else if (route === '/tutorial') {
    await renderRoute(() => startTutorialGame());
  } else if (route === '/museum') {
    if (!museumVisible) await renderRoute(() => showMuseum());
  } else if (parts[0] === 'museum' && parts[1]) {
    if (!museumVisible) await renderRoute(() => showMuseum());
    if (!isCurrentRequest()) return getCurrentAppRoute();
    // A truncated link (e.g. "#/museum/%") must not throw and leave the app blank.
    let entryName = null;
    try { entryName = decodeURIComponent(parts.slice(1).join('/')); } catch (_error) {}
    // Unknown or still-locked genera stay on the Museum (the entry shows the lineage).
    const discoveredRecords = typeof museumDiscoveryRecords === 'object' ? museumDiscoveryRecords : null;
    const entryUnlocked = entryName !== null && (
      !discoveredRecords || Boolean(discoveredRecords[entryName.toLowerCase()])
      || (typeof isAnalyticsAdmin !== 'undefined' && isAnalyticsAdmin === true));
    if (!entryUnlocked) {
      route = '/museum';
    } else {
      await renderRoute(() => showMuseumEntry(entryName));
    }
  } else if (route === '/cladogram') {
    if (!document.querySelector('#app-content .cladogram-viewport')) await renderRoute(() => showCladogram());
  } else if (route === '/practice') {
    renderRoute(() => showPracticeMode());
  } else if (route === '/friends') {
    renderRoute(() => showFriendChallenges());
  } else if (route === '/about') {
    await renderRoute(() => showAbout());
  } else if (route === '/stats' && currentUser) {
    await renderRoute(() => showStatsDashboard());
  } else if (route === '/account' && currentUser) {
    await renderRoute(() => showAccountPanel());
  } else if (route === '/analytics' && currentUser && await initializeAnalyticsAccess()) {
    if (!isCurrentRequest()) return getCurrentAppRoute();
    await renderRoute(() => showAnalyticsDashboard());
  } else if ((parts[0] === 'game' || parts[0] === 'challenge')
      && typeof isGameRouteMounted === 'function' && isGameRouteMounted(route)) {
    // Overlays above the game were closed above; the game itself stays as is.
  } else if (parts[0] === 'game' && parts.length === 3
      && ['daily', 'practice'].includes(parts[1])
      && (APP_ROUTE_DIFFICULTIES.has(routeSegmentToLevel(parts[2]))
        || (parts[1] === 'practice' && parseCustomPoolKey(routeSegmentToLevel(parts[2]))))) {
    const level = routeSegmentToLevel(parts[2]);
    route = `/game/${parts[1]}/${levelToRouteSegment(level)}`;
    if (parts[1] === 'practice') {
      await renderRoute(() => startPracticeChallenge(level, { restoreExisting: true }));
    } else {
      await renderRoute(() => startDailyChallenge(level, { restoreExisting: true }));
    }
  } else if (parts[0] === 'challenge' && parts[1]) {
    const restored = await restoreStoredChallenge(parts[1], {
      isCurrentRequest,
      renderChallenge: data => renderRoute(() => startFriendChallengeFromPayload(data))
    });
    if (!isCurrentRequest()) return getCurrentAppRoute();
    if (!restored) {
      route = '/friends';
      renderRoute(() => showFriendChallenges(parts[1]));
    }
  } else {
    route = '/';
    await renderRoute(() => showDifficultySelection());
  }
  if (!isCurrentRequest()) return getCurrentAppRoute();

  window.history.replaceState({
    ...(window.history.state || {}),
    phylosaurRoute: route,
    phylosaurDepth: Number(window.history.state?.phylosaurDepth || 0)
  }, document.title, buildAppRouteUrl(route));

  return route;
}

window.addEventListener('popstate', () => {
  const depth = Number(window.history.state?.phylosaurDepth || 0);
  trackNavigation(depth < lastRouteDepth ? 'back' : 'forward', depth);
  if (appRoutingReady) restoreAppRoute();
});

if ('scrollRestoration' in window.history) window.history.scrollRestoration = 'manual';

window.addEventListener('pagehide', () => {
  try {
    sessionStorage.setItem(PHYLOSAUR_STORAGE_KEYS.reloadScroll,
      JSON.stringify({ url: window.location.href, y: Math.round(window.scrollY) }));
  } catch (_error) {}
});

function restoreReloadScrollPosition() {
  let saved = null;
  try {
    saved = JSON.parse(sessionStorage.getItem(PHYLOSAUR_STORAGE_KEYS.reloadScroll) || 'null');
    sessionStorage.removeItem(PHYLOSAUR_STORAGE_KEYS.reloadScroll);
  } catch (_error) {}
  const navigation = performance.getEntriesByType?.('navigation')?.[0];
  if (!saved || saved.url !== window.location.href || !(saved.y > 0)
      || (navigation && navigation.type !== 'reload')) return;

  const interactions = ['wheel', 'touchstart', 'keydown', 'mousedown'];
  let cancelled = false;
  const cancel = () => { cancelled = true; };
  const stop = () => interactions.forEach(type => window.removeEventListener(type, cancel));
  interactions.forEach(type => window.addEventListener(type, cancel, { passive: true }));
  const deadline = performance.now() + 5000;
  const attempt = () => {
    if (cancelled) return stop();
    const maxY = document.documentElement.scrollHeight - window.innerHeight;
    if (maxY >= saved.y || performance.now() > deadline) {
      window.scrollTo({ top: Math.min(saved.y, Math.max(0, maxY)), behavior: 'instant' });
      return stop();
    }
    requestAnimationFrame(attempt);
  };
  requestAnimationFrame(attempt);
}

window.addEventListener('resize', () => {
  const nextWidth = window.innerWidth;
  if (nextWidth === lastTreeViewportWidth) return;
  lastTreeViewportWidth = nextWidth;

  clearTimeout(treeResizeTimer);
  treeResizeTimer = setTimeout(() => {
    if (document.getElementById('tree-svg')) renderCurrentGameTree();
  }, 150);
});

document.addEventListener('DOMContentLoaded', async function() {
  initializeI18n();
  if (typeof watchGlassSelects === 'function') watchGlassSelects();  let savedTheme = 'dark';
  try {
    savedTheme = localStorage.getItem(PHYLOSAUR_STORAGE_KEYS.theme) || 'dark';
  } catch (_error) {}
  applyTheme(savedTheme, { persist: false });
  restoreReloadScrollPosition();

  if (!sb) {
    window.showPhylosaurBootError?.();
    return;
  }
  window.phylosaurBooted = true;
  watchExternalSignOut();

  const hash = window.location.hash;
  const params = new URLSearchParams(hash.replace('#', ''));

  if (params.get('error')) {
      await initializeUserSystem();
      appRoutingReady = true;
      showDifficultySelection();
      setTimeout(() => {
      showLoginModal();
      setTimeout(() => {
          const el = document.getElementById('signin-global-error');
          if (el) {
          el.textContent = t('auth.resetExpired');
          el.classList.add('visible');
          }
          window.history.replaceState({}, document.title, window.location.pathname);
      }, 100);
      }, 100);
      return;
  }

  if (params.get('type') === 'recovery') {
      await initializeUserSystem();
      appRoutingReady = true;
      showPasswordUpdateForm();
      window.history.replaceState({}, document.title, window.location.pathname);
      return;
  }

  const userInitialization = initializeUserSystem();
  accountInitPromise = userInitialization.catch(() => null);
  appRoutingReady = true;
  const challengeCode = new URLSearchParams(window.location.search).get('challenge');
  if (challengeCode) {
    // A shared link is consumed once; later routes and reloads must not reopen it.
    const url = new URL(window.location.href);
    url.searchParams.delete('challenge');
    window.history.replaceState(window.history.state, document.title, url.href);
  }
  let restoredRoute;
  if (!challengeCode && getCurrentAppRoute() === '/') {
    await showDifficultySelection();
    restoredRoute = '/';
    void userInitialization.then(() => {
      if (getCurrentAppRoute() === '/') return refreshDifficultySelectionAccountState();
    });
  } else {
    await userInitialization;
  }

  if (challengeCode && getCurrentAppRoute() === '/') {
    showFriendChallenges(challengeCode);
    restoredRoute = '/friends';
  } else if (!restoredRoute) {
    restoredRoute = await restoreAppRoute();
  }
  // Screens render their header before admin access is known; the home screen
  // refreshes its own, every other restored screen is updated here.
  if (currentUser && restoredRoute !== '/') {
    void initializeAnalyticsAccess()
      .then(isAdmin => { if (isAdmin) refreshHeaderControls(); })
      .catch(() => {});
  }
});