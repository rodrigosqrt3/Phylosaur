const sb = window.supabase.createClient(SUPABASE_URL, SUPABASE_ANON_KEY);

let lastTreeViewportWidth = window.innerWidth;
let treeResizeTimer = null;
let isRestoringAppRoute = false;
let appRouteRestoreGeneration = 0;
let appRoutingReady = false;

const APP_ROUTE_DIFFICULTIES = new Set([
  'muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil'
]);

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

  window.history[shouldReplace ? 'replaceState' : 'pushState'](
    nextState,
    document.title,
    buildAppRouteUrl(normalized)
  );
}

function closeTransientRouteOverlays({ closeMuseum = true } = {}) {
  let removedUnmanagedOverlay = false;
  Array.from(document.querySelectorAll('.modal-overlay, .tutorial-overlay')).reverse().forEach(element => {
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
  } else if (route === '/museum') {
    if (!museumVisible) await renderRoute(() => showMuseum());
  } else if (parts[0] === 'museum' && parts[1]) {
    if (!museumVisible) await renderRoute(() => showMuseum());
    if (!isCurrentRequest()) return getCurrentAppRoute();
    await renderRoute(() => showMuseumEntry(decodeURIComponent(parts.slice(1).join('/'))));
  } else if (route === '/practice') {
    renderRoute(() => showPracticeMode());
  } else if (route === '/friends') {
    renderRoute(() => showFriendChallenges());
  } else if (route === '/about') {
    await renderRoute(() => showAbout());
  } else if (route === '/stats' && currentUser) {
    await renderRoute(() => showStatsDashboard());
  } else if (route === '/analytics' && isAnalyticsAdmin) {
    await renderRoute(() => showAnalyticsDashboard());
  } else if (parts[0] === 'game' && parts.length === 3
      && ['daily', 'practice'].includes(parts[1])
      && APP_ROUTE_DIFFICULTIES.has(parts[2])) {
    if (parts[1] === 'practice') {
      await renderRoute(() => startPracticeChallenge(parts[2], { restoreExisting: true }));
    } else {
      await renderRoute(() => startDailyChallenge(parts[2], { restoreExisting: true }));
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
  if (appRoutingReady) restoreAppRoute();
});

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
  let savedTheme = 'dark';
  try {
    savedTheme = localStorage.getItem(PHYLOSAUR_STORAGE_KEYS.theme) || 'dark';
  } catch (_error) {}
  applyTheme(savedTheme, { persist: false });

  const hash = window.location.hash;
  const params = new URLSearchParams(hash.replace('#', ''));

  if (params.get('error')) {
      await initializeUserSystem();
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
      showPasswordUpdateForm();
      window.history.replaceState({}, document.title, window.location.pathname);
      return;
  }

  const userInitialization = initializeUserSystem();
  appRoutingReady = true;
  const challengeCode = new URLSearchParams(window.location.search).get('challenge');
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

  if (restoredRoute === '/') maybeShowFirstRunTutorial();
});