const sb = window.supabase.createClient(SUPABASE_URL, SUPABASE_ANON_KEY);

let lastTreeViewportWidth = window.innerWidth;
let treeResizeTimer = null;

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
  const challengeCode = new URLSearchParams(window.location.search).get('challenge');
  if (challengeCode) {
    showFriendChallenges(challengeCode);
  } else {
    showDifficultySelection();
  }
});