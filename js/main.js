// ═══════════════════════════════════════════════
//  _____ _     _       _____
// |  __ (_)   | |     / ____|
// | |__) |  _| |_   | (___   ___ _ ____   _____ _ __       
// |  ___/ |/ / __|   \___ \ / _ \ '__\ \ / / _ \ '__|
// | |   |   <| |_    ____) |  __/ |   \ V /  __/ |
// |_|   |_|\_\\__|  |_____/ \___|_|    \_/ \___|_|
// ═══════════════════════════════════════════════

const sb = window.supabase.createClient(SUPABASE_URL, SUPABASE_ANON_KEY);

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
  showDifficultySelection();
});