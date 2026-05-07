const sb = window.supabase.createClient(SUPABASE_URL, SUPABASE_ANON_KEY);

document.addEventListener('DOMContentLoaded', async function() {
    
  const urlParams = new URLSearchParams(window.location.search);
  const isTotalDino = urlParams.get('partner') === 'totaldino';

  if (isTotalDino) {
      document.body.classList.add('totaldino-mode');
      sessionStorage.setItem('partner', 'totaldino'); 
  } 
  else if (sessionStorage.getItem('partner') === 'totaldino') {
      document.body.classList.add('totaldino-mode');
  } 
  else {
      const savedTheme = localStorage.getItem('phylosaur-theme');
      if (savedTheme === 'light') toggleTheme();
  }

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