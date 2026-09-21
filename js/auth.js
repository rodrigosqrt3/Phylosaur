// ═══════════════════════════════════════════════
// USER ACCOUNT SYSTEM
// ═══════════════════════════════════════════════
async function initializeUserSystem() {
  try {
    const { data: { session } } = await sb.auth.getSession();
    if (!session) return null;

    currentUserId = session.user.id;
    analyticsAccessChecked = false;
    const [profileResult, statsResult] = await Promise.all([
      sb.from('profiles').select('username').eq('id', currentUserId).single(),
      sb.from('statistics').select('*').eq('user_id', currentUserId).single()
    ]);
    const profile = profileResult.data;
    const stats = statsResult.data;

    currentUser = profile?.username || session.user.email?.split('@')[0] || null;

    if (stats) {
      userStats.gamesPlayed = stats.games_played;
      userStats.gamesWon = stats.games_won;
      userStats.totalGuesses = stats.total_guesses;
      userStats.bestScore = stats.best_score;
    }

    void claimGuestProgressOnLogin({ showNotice: false })
      .then(() => {
        if (typeof getCurrentAppRoute === 'function' && getCurrentAppRoute() === '/') {
          return refreshDifficultySelectionAccountState();
        }
      })
      .catch(error => console.warn('Background account synchronization failed:', error));

    return session;
  } catch (error) {
    console.warn('Account initialization could not be completed:', error);
    return null;
  }
}

async function initUserStatsRow() {
  if (!currentUserId) return;

  const { data: current } = await sb.from('statistics')
    .select('user_id')
    .eq('user_id', currentUserId)
    .single();

  if (current) return;

  await sb.from('statistics').insert({
    user_id:       currentUserId,
    games_played:  0,
    games_won:     0,
    total_guesses: 0,
    best_score:    null,
    updated_at:    new Date().toISOString()
  });
}

function showLoginScreen() {
  setHeaderControls('login');
  const appContent = document.getElementById('app-content');

  appContent.innerHTML = `
    <div class="game-card login-card">
      <div class="tab-row">
        <button class="tab-btn active" id="tab-signin" onclick="loginSwitchTab('signin')">Sign In</button>
        <button class="tab-btn" id="tab-register" onclick="loginSwitchTab('register')">Create Account</button>
      </div>

      <div class="login-form-panel active" id="login-panel-signin">
        <div class="login-global-error" id="signin-global-error"></div>
        <div class="login-global-success" id="signin-global-success"></div>
        <div class="login-field">
          <label for="signin-email">Email</label>
          <input type="email" id="signin-email" placeholder="your@email.com" autocomplete="email" />
          <div class="login-field-error" id="signin-email-err">Please enter a valid email address.</div>
        </div>
        <div class="login-field">
          <label for="signin-password">Password</label>
          <input type="password" id="signin-password" placeholder="••••••••" autocomplete="current-password" />
          <div class="login-field-error" id="signin-password-err">Password is required.</div>
        </div>
        <button type="button" class="login-forgot login-text-action" onclick="loginShowReset()">Forgot password?</button>
        <button class="btn-guess btn-block btn-large" id="signin-btn" onclick="handleSignIn()">
          Sign In
        </button>
      </div>

      <div class="login-form-panel" id="login-panel-register">
        <div class="login-global-error" id="register-global-error"></div>
        <div class="login-global-success" id="register-global-success"></div>
        <div class="login-field">
          <label for="reg-email">Email</label>
          <input type="email" id="reg-email" placeholder="your@email.com" autocomplete="email" />
          <div class="login-field-error" id="reg-email-err">Please enter a valid email address.</div>
        </div>
        <div class="login-field">
          <label for="reg-password">Password</label>
          <input type="password" id="reg-password" placeholder="At least 6 characters" autocomplete="new-password" />
          <div class="login-field-error" id="reg-password-err">Password must be at least 6 characters.</div>
        </div>
        <div class="login-field login-field-last">
          <label for="reg-confirm">Confirm Password</label>
          <input type="password" id="reg-confirm" placeholder="Repeat password" autocomplete="new-password" />
          <div class="login-field-error" id="reg-confirm-err">Passwords do not match.</div>
        </div>
        <button class="btn-guess btn-block btn-large" id="register-btn" onclick="handleRegister()">
          Create Account
        </button>
      </div>

      <div class="login-form-panel" id="login-panel-reset">
        <button type="button" class="login-text-action login-back-link" onclick="loginShowReset(false)"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back to sign in</span></button>
        <p class="login-reset-title">Reset Password</p>
        <p class="login-reset-copy">
          Enter your email and we'll send you a reset link.
        </p>
        <div class="login-global-error" id="reset-global-error"></div>
        <div class="login-global-success" id="reset-global-success"></div>
        <div class="login-field login-field-last">
          <label for="reset-email">Email</label>
          <input type="email" id="reset-email" placeholder="your@email.com" />
          <div class="login-field-error" id="reset-email-err">Please enter a valid email address.</div>
        </div>
        <button class="btn-guess btn-block btn-large" id="reset-btn" onclick="handleReset()">
          Send Reset Link
        </button>
      </div>

      <p class="login-guest-action">
        <button onclick="continueAsGuest()" class="btn-hint btn-block btn-large btn-spaced">
          Play Without Account
        </button>
      </p>
    </div>
  `;

  document.addEventListener('keydown', loginEnterHandler);
}

let activeLoginModalCleanup = null;

function showLoginModal() {
  closeLoginModal();
  const previouslyFocused = document.activeElement;
  const previousBodyOverflow = document.body.style.overflow;
  const overlay = document.createElement('div');
  overlay.className = 'modal-overlay';
  overlay.id = 'login-modal-overlay';
  overlay.setAttribute('role', 'dialog');
  overlay.setAttribute('aria-modal', 'true');
  overlay.setAttribute('aria-label', 'Account access');
  
  const box = document.createElement('div');
  box.className = 'modal-box login-modal-box';
  box.tabIndex = -1;
  box.style.maxWidth = '480px';
  box.style.width = '90%';
  
  box.innerHTML = `
    <div class="tab-row">
      <button class="tab-btn active" id="tab-signin" onclick="loginSwitchTab('signin')">Sign In</button>
      <button class="tab-btn" id="tab-register" onclick="loginSwitchTab('register')">Create Account</button>
    </div>

    <div class="login-form-panel active" id="login-panel-signin">
      <div class="login-global-error" id="signin-global-error"></div>
      <div class="login-global-success" id="signin-global-success"></div>
      <div class="login-field">
        <label for="signin-email">Email</label>
        <input type="email" id="signin-email" placeholder="your@email.com" autocomplete="email" />
        <div class="login-field-error" id="signin-email-err">Please enter a valid email address.</div>
      </div>
      <div class="login-field">
        <label for="signin-password">Password</label>
        <input type="password" id="signin-password" placeholder="••••••••" autocomplete="current-password" />
        <div class="login-field-error" id="signin-password-err">Password is required.</div>
      </div>
      <button type="button" class="login-forgot login-text-action" onclick="loginShowReset()">Forgot password?</button>
      <button class="btn-guess btn-block btn-large" id="signin-btn" onclick="handleSignInModal()">
        Sign In
      </button>
    </div>

    <div class="login-form-panel" id="login-panel-register">
      <div class="login-global-error" id="register-global-error"></div>
      <div class="login-global-success" id="register-global-success"></div>
      <div class="login-field">
        <label for="reg-email">Email</label>
        <input type="email" id="reg-email" placeholder="your@email.com" autocomplete="email" />
        <div class="login-field-error" id="reg-email-err">Please enter a valid email address.</div>
      </div>
      <div class="login-field">
        <label for="reg-password">Password</label>
        <input type="password" id="reg-password" placeholder="At least 6 characters" autocomplete="new-password" />
        <div class="login-field-error" id="reg-password-err">Password must be at least 6 characters.</div>
      </div>
      <div class="login-field login-field-last">
        <label for="reg-confirm">Confirm Password</label>
        <input type="password" id="reg-confirm" placeholder="Repeat password" autocomplete="new-password" />
        <div class="login-field-error" id="reg-confirm-err">Passwords do not match.</div>
      </div>
      <button class="btn-guess btn-block btn-large" id="register-btn" onclick="handleRegisterModal()">
        Create Account
      </button>
    </div>

    <div class="login-form-panel" id="login-panel-reset">
      <button type="button" class="login-text-action login-back-link" onclick="loginShowReset(false)"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back to sign in</span></button>
      <div class="login-global-error" id="reset-global-error"></div>
      <div class="login-global-success" id="reset-global-success"></div>
      <div class="login-field login-field-last">
        <label for="reset-email">Email</label>
        <input type="email" id="reset-email" placeholder="your@email.com" />
        <div class="login-field-error" id="reset-email-err">Please enter a valid email address.</div>
      </div>
      <button class="btn-guess btn-block btn-large" id="reset-btn" onclick="handleReset()">
        Send Reset Link
      </button>
    </div>

    <button onclick="closeLoginModal()" class="btn-hint btn-block btn-spaced">
      Continue Without Account
    </button>
  `;

  overlay.appendChild(box);
  document.body.appendChild(overlay);
  document.body.style.overflow = 'hidden';

  const modalKeyHandler = event => {
    if (event.key === 'Escape') {
      event.preventDefault();
      closeLoginModal();
      return;
    }
    if (event.key !== 'Tab') return;
    const focusable = Array.from(box.querySelectorAll(
      'button:not([disabled]), input:not([disabled]), a[href], [tabindex]:not([tabindex="-1"])'
    ));
    if (!focusable.length) {
      event.preventDefault();
      box.focus();
      return;
    }
    const first = focusable[0];
    const last = focusable[focusable.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    }
  };

  document.addEventListener('keydown', modalKeyHandler, true);
  activeLoginModalCleanup = () => {
    document.removeEventListener('keydown', modalKeyHandler, true);
    document.body.style.overflow = previousBodyOverflow;
    if (previouslyFocused instanceof HTMLElement && previouslyFocused.isConnected) {
      previouslyFocused.focus();
    }
  };

  overlay.addEventListener('click', e => {
    if (e.target === overlay) closeLoginModal();
  });

  queueMicrotask(() => document.getElementById('signin-email')?.focus() || box.focus());
}

function closeLoginModal() {
  const overlay = document.getElementById('login-modal-overlay');
  overlay?.remove();
  activeLoginModalCleanup?.();
  activeLoginModalCleanup = null;
}

async function handleSignInModal() {
  loginClearErrors();
  const email    = document.getElementById('signin-email')?.value.trim();
  const password = document.getElementById('signin-password')?.value;
  let valid = true;

  if (!loginIsValidEmail(email))  { loginShowFieldError('signin-email-err');    valid = false; }
  if (!password)                  { loginShowFieldError('signin-password-err'); valid = false; }
  if (!valid) return;

  loginSetLoading('signin-btn', true);

  const { data, error } = await sb.auth.signInWithPassword({ email, password });

  if (error) {
    loginShowGlobalError('signin', 'Invalid email or password. Please try again.');
    loginSetLoading('signin-btn', false);
    return;
  }

  const { data: profile } = await sb.from('profiles')
    .select('username')
    .eq('id', data.user.id)
    .single();

  currentUser   = profile?.username || email.split('@')[0];
  currentUserId = data.user.id;
  analyticsAccessChecked = false;
  await initUserStatsRow();
  await claimGuestProgressOnLogin({ showNotice: true });

  closeLoginModal();
  setHeaderControls(selectedDifficulty ? 'game' : 'difficulty');
}

async function handleRegisterModal() {
  loginClearErrors();
  const email    = document.getElementById('reg-email')?.value.trim();
  const password = document.getElementById('reg-password')?.value;
  const confirm  = document.getElementById('reg-confirm')?.value;
  let valid = true;

  if (!loginIsValidEmail(email))   { loginShowFieldError('reg-email-err');    valid = false; }
  if (password.length < 6)         { loginShowFieldError('reg-password-err'); valid = false; }
  if (password !== confirm)        { loginShowFieldError('reg-confirm-err');  valid = false; }
  if (!valid) return;

  loginSetLoading('register-btn', true);

  const { data, error } = await sb.auth.signUp({ email, password });

  if (error) {
    loginShowGlobalError('register', error.message);
    loginSetLoading('register-btn', false);
    return;
  }

  if (data.user) {
    await sb.from('profiles').insert({ 
      id: data.user.id, 
      username: email.split('@')[0] 
    });
  }

  loginSetLoading('register-btn', false);
  loginShowGlobalSuccess('register', 'Account created! Check your email to confirm, then sign in.');
}

function loginSwitchTab(tab) {
  document.querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
  document.querySelectorAll('.login-form-panel').forEach(p => p.classList.remove('active'));
  document.getElementById('tab-' + tab).classList.add('active');
  document.getElementById('login-panel-' + tab).classList.add('active');
  loginClearErrors();
}

function loginShowReset(show = true) {
  document.querySelectorAll('.login-form-panel').forEach(p => p.classList.remove('active'));
  document.querySelector('.tab-row').style.display = show ? 'none' : 'grid';
  if (show) {
    document.getElementById('login-panel-reset').classList.add('active');
  } else {
    document.getElementById('login-panel-signin').classList.add('active');
    document.querySelector('.tab-row').style.display = 'grid';
  }
}

function loginClearErrors() {
  document.querySelectorAll('.login-field-error').forEach(e => e.classList.remove('visible'));
  document.querySelectorAll('.login-field input').forEach(i => i.classList.remove('input-error'));
  document.querySelectorAll('.login-global-error, .login-global-success').forEach(e => e.classList.remove('visible'));
}

function loginShowFieldError(errId) {
  const err = document.getElementById(errId);
  if (!err) return;
  err.classList.add('visible');
  err.previousElementSibling.classList.add('input-error');
}

function loginShowGlobalError(panelPrefix, msg) {
  const el = document.getElementById(panelPrefix + '-global-error');
  if (!el) return;
  el.textContent = msg;
  el.classList.add('visible');
}

function loginShowGlobalSuccess(panelPrefix, msg) {
  const el = document.getElementById(panelPrefix + '-global-success');
  if (!el) return;
  el.textContent = msg;
  el.classList.add('visible');
}

function loginSetLoading(btnId, loading) {
  const btn = document.getElementById(btnId);
  if (!btn) return;
  btn.disabled = loading;
  btn.classList.toggle('btn-loading', loading);
}

function loginIsValidEmail(e) { 
  return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(e); 
}

function loginEnterHandler(e) {
  if (e.key !== 'Enter') return;
  const active = document.querySelector('.login-form-panel.active');
  if (!active) return;
  const id = active.id;
  if (id === 'login-panel-signin')        handleSignIn();
  else if (id === 'login-panel-register') handleRegister();
  else if (id === 'login-panel-reset')    handleReset();
}

async function handleSignIn() {
  loginClearErrors();
  const email    = document.getElementById('signin-email')?.value.trim();
  const password = document.getElementById('signin-password')?.value;
  let valid = true;

  if (!loginIsValidEmail(email))  { loginShowFieldError('signin-email-err');    valid = false; }
  if (!password)                  { loginShowFieldError('signin-password-err'); valid = false; }
  if (!valid) return;

  loginSetLoading('signin-btn', true);

  let { data, error } = await sb.auth.signInWithPassword({ email, password });

  if (error) {
    if (error.message.includes('Invalid login credentials')) {
      loginShowGlobalError('signin', 'Invalid email or password. Please try again.');
    } else {
      loginShowGlobalError('signin', error.message);
    }
    loginSetLoading('signin-btn', false);
    return;
  }

  const { data: profile } = await sb.from('profiles')
    .select('username')
    .eq('id', data.user.id)
    .single();

  currentUser   = profile?.username || email.split('@')[0];
  currentUserId = data.user.id;
  analyticsAccessChecked = false;
  await initUserStatsRow();
  await claimGuestProgressOnLogin({ showNotice: true });

  document.removeEventListener('keydown', loginEnterHandler);
  showDifficultySelection();
}

async function handleRegister() {
  loginClearErrors();
  const email    = document.getElementById('reg-email')?.value.trim();
  const password = document.getElementById('reg-password')?.value;
  const confirm  = document.getElementById('reg-confirm')?.value;
  let valid = true;

  if (!loginIsValidEmail(email))   { loginShowFieldError('reg-email-err');    valid = false; }
  if (password.length < 6)         { loginShowFieldError('reg-password-err'); valid = false; }
  if (password !== confirm)        { loginShowFieldError('reg-confirm-err');  valid = false; }
  if (!valid) return;

  loginSetLoading('register-btn', true);

  const { data, error } = await sb.auth.signUp({ email, password });

  if (error) {
    loginShowGlobalError('register', error.message);
    loginSetLoading('register-btn', false);
    return;
  }

  if (data.user) {
    const usernameFromEmail = email.split('@')[0];
    await sb.from('profiles').insert({ id: data.user.id, username: usernameFromEmail });
  }

  loginSetLoading('register-btn', false);
  loginShowGlobalSuccess('register', 'Account created! Check your email to confirm, then sign in.');
}

async function handleReset() {
  loginClearErrors();
  const email = document.getElementById('reset-email')?.value.trim();

  if (!loginIsValidEmail(email)) { loginShowFieldError('reset-email-err'); return; }

  loginSetLoading('reset-btn', true);

  const { error } = await sb.auth.resetPasswordForEmail(email, {
    redirectTo: window.location.origin
  });

  loginSetLoading('reset-btn', false);

  if (error) {
    loginShowGlobalError('reset', error.message);
  } else {
    loginShowGlobalSuccess('reset', 'Reset link sent! Check your inbox.');
  }
}

function continueAsGuest() {
  currentUser = null;
  currentUserId = null;
  isAnalyticsAdmin = false;
  analyticsAccessChecked = true;
  showDifficultySelection();
}

async function logout() {
  const confirm = await customConfirm(
    'Confirm Logout',
    'Are you sure you want to sign out?',
    'Logout',
    'Cancel'
  );

  if (confirm === 'true') {
    await sb.auth.signOut();
    localStorage.removeItem('phylosaur-discoveries');
    currentUser = null;
    currentUserId = null;
    isAnalyticsAdmin = false;
    analyticsAccessChecked = true;
    userStats = {
      gamesPlayed: 0,
      gamesWon: 0,
      totalGuesses: 0,
      bestScore: null,
      difficultyStats: {
        'muito_facil':   { played: 0, won: 0, avgGuesses: 0 },
        'facil':         { played: 0, won: 0, avgGuesses: 0 },
        'normal':        { played: 0, won: 0, avgGuesses: 0 },
        'dificil':       { played: 0, won: 0, avgGuesses: 0 },
        'muito_dificil': { played: 0, won: 0, avgGuesses: 0 }
      },
      recentGames: [],
      achievements: []
    };
    showDifficultySelection();
  }
}

function showPasswordUpdateForm() {
  const appContent = document.getElementById('app-content');

  appContent.innerHTML = `
    <div class="game-card" style="max-width:480px; margin:40px auto; padding:40px;">
      <div style="text-align:center; margin-bottom:32px;">
        <h1 style="font-size:2.8em; color:#d4b87e; font-weight:300; letter-spacing:14px; text-transform:uppercase; font-variant:small-caps; text-shadow:2px 2px 8px rgba(0,0,0,0.6); margin-bottom:6px;">Phylosaur</h1>
      </div>
      <p style="color:#d4b87e; font-size:1.1em; letter-spacing:2px; margin-bottom:10px;">Set New Password</p>
      <p style="color:#8b7355; font-size:0.88em; line-height:1.7; margin-bottom:28px; font-style:italic;">
        Choose a new password for your account.
      </p>
      <div class="login-global-error" id="update-global-error"></div>
      <div class="login-global-success" id="update-global-success"></div>
      <div class="login-field">
        <label>New Password</label>
        <input type="password" id="update-password" placeholder="At least 6 characters" />
        <div class="login-field-error" id="update-password-err">Password must be at least 6 characters.</div>
      </div>
      <div class="login-field" style="margin-bottom:28px;">
        <label>Confirm Password</label>
        <input type="password" id="update-confirm" placeholder="Repeat password" />
        <div class="login-field-error" id="update-confirm-err">Passwords do not match.</div>
      </div>
      <button class="btn-guess btn-block btn-large" id="update-btn" onclick="handlePasswordUpdate()">
        Update Password
      </button>
    </div>
  `;
}

async function handlePasswordUpdate() {
  loginClearErrors();
  const password = document.getElementById('update-password')?.value;
  const confirm  = document.getElementById('update-confirm')?.value;
  let valid = true;

  if (password.length < 6) { loginShowFieldError('update-password-err'); valid = false; }
  if (password !== confirm) { loginShowFieldError('update-confirm-err');  valid = false; }
  if (!valid) return;

  loginSetLoading('update-btn', true);

  const { error } = await sb.auth.updateUser({ password });

  loginSetLoading('update-btn', false);

  if (error) {
    const el = document.getElementById('update-global-error');
    el.textContent = error.message;
    el.classList.add('visible');
    return;
  }

  const el = document.getElementById('update-global-success');
  el.textContent = 'Password updated! Redirecting…';
  el.classList.add('visible');
  window.history.replaceState({}, document.title, window.location.pathname);

  setTimeout(() => showDifficultySelection(), 1500);
}