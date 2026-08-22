const sb = window.supabase.createClient(SUPABASE_URL, SUPABASE_ANON_KEY);

let lastTreeViewportWidth = window.innerWidth;
let treeResizeTimer = null;
let isRestoringAppRoute = false;
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

function closeTransientRouteOverlays() {
  if (typeof closeMuseumEntry === 'function') closeMuseumEntry();
  document.querySelectorAll('.modal-overlay, .tutorial-overlay').forEach(element => element.remove());
  document.body.style.overflow = '';
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

async function restoreAppRoute() {
  let route = getCurrentAppRoute();
  const parts = route.split('/').filter(Boolean);

  closeTransientRouteOverlays();
  isRestoringAppRoute = true;

  try {
    if (route === '/') {
      await showDifficultySelection();
    } else if (route === '/museum') {
      await showMuseum();
    } else if (parts[0] === 'museum' && parts[1]) {
      await showMuseum();
      await showMuseumEntry(decodeURIComponent(parts.slice(1).join('/')));
    } else if (route === '/practice') {
      showPracticeMode();
    } else if (route === '/friends') {
      showFriendChallenges();
    } else if (route === '/about') {
      showAbout();
    } else if (route === '/stats' && currentUser) {
      await showStatsDashboard();
    } else if (route === '/analytics' && isAnalyticsAdmin) {
      await showAnalyticsDashboard();
    } else if (parts[0] === 'game' && parts.length === 3
        && ['daily', 'practice'].includes(parts[1])
        && APP_ROUTE_DIFFICULTIES.has(parts[2])) {
      if (parts[1] === 'practice') {
        await startPracticeChallenge(parts[2], { restoreExisting: true });
      } else {
        await startDailyChallenge(parts[2], { restoreExisting: true });
      }
    } else if (parts[0] === 'challenge' && parts[1]) {
      const restored = await restoreStoredChallenge(parts[1]);
      if (!restored) {
        route = '/friends';
        showFriendChallenges(parts[1]);
      }
    } else {
      route = '/';
      await showDifficultySelection();
    }
  } finally {
    isRestoringAppRoute = false;
  }

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
  const savedTheme = localStorage.getItem('phylosaur-theme');
  if (savedTheme === 'light') toggleTheme();

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
          el.textContent = 'Your reset link has expired. Please request a new one.';
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

  await initializeUserSystem();
  appRoutingReady = true;
  const challengeCode = new URLSearchParams(window.location.search).get('challenge');
  let restoredRoute;
  if (challengeCode && getCurrentAppRoute() === '/') {
    showFriendChallenges(challengeCode);
    restoredRoute = '/friends';
  } else {
    restoredRoute = await restoreAppRoute();
  }

  if (restoredRoute === '/') maybeShowFirstRunTutorial();
});