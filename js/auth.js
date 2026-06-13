// ═══════════════════════════════════════════════
// USER ACCOUNT SYSTEM
// ═══════════════════════════════════════════════
async function initializeUserSystem() {
  const { data: { session } } = await sb.auth.getSession();
  
  if (session) {
    currentUserId = session.user.id;
    
    const { data: profile } = await sb.from('profiles')
      .select('username')
      .eq('id', currentUserId)
      .single();
    
    if (profile) {
      currentUser = profile.username;
      
      const { data: stats } = await sb.from('statistics')
        .select('*')
        .eq('user_id', currentUserId)
        .single();
      
      if (stats) {
        userStats.gamesPlayed = stats.games_played;
        userStats.gamesWon = stats.games_won;
        userStats.totalGuesses = stats.total_guesses;
        userStats.bestScore = stats.best_score;
      }
    }
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
    <div class="game-card" style="max-width:480px; margin:40px auto; padding:40px;">
      <div class="tab-row">
        <button class="tab-btn active" id="tab-signin" onclick="loginSwitchTab('signin')">Sign In</button>
        <button class="tab-btn" id="tab-register" onclick="loginSwitchTab('register')">Create Account</button>
      </div>

      <div class="login-form-panel active" id="login-panel-signin">
        <div class="login-global-error" id="signin-global-error"></div>
        <div class="login-global-success" id="signin-global-success"></div>
        <div class="login-field">
          <label>Email</label>
          <input type="email" id="signin-email" placeholder="your@email.com" autocomplete="email" />
          <div class="login-field-error" id="signin-email-err">Please enter a valid email address.</div>
        </div>
        <div class="login-field">
          <label>Password</label>
          <input type="password" id="signin-password" placeholder="••••••••" autocomplete="current-password" />
          <div class="login-field-error" id="signin-password-err">Password is required.</div>
        </div>
        <span class="login-forgot" onclick="loginShowReset()">Forgot password?</span>
        <button class="btn-guess" id="signin-btn" onclick="handleSignIn()" style="width:100%; padding:15px; font-size:14px; letter-spacing:2px;">
          Begin Classification
        </button>
      </div>

      <div class="login-form-panel" id="login-panel-register">
        <div class="login-global-error" id="register-global-error"></div>
        <div class="login-global-success" id="register-global-success"></div>
        <div class="login-field">
          <label>Email</label>
          <input type="email" id="reg-email" placeholder="your@email.com" autocomplete="email" />
          <div class="login-field-error" id="reg-email-err">Please enter a valid email address.</div>
        </div>
        <div class="login-field">
          <label>Password</label>
          <input type="password" id="reg-password" placeholder="At least 6 characters" autocomplete="new-password" />
          <div class="login-field-error" id="reg-password-err">Password must be at least 6 characters.</div>
        </div>
        <div class="login-field" style="margin-bottom:28px;">
          <label>Confirm Password</label>
          <input type="password" id="reg-confirm" placeholder="Repeat password" autocomplete="new-password" />
          <div class="login-field-error" id="reg-confirm-err">Passwords do not match.</div>
        </div>
        <button class="btn-guess" id="register-btn" onclick="handleRegister()" style="width:100%; padding:15px; font-size:14px; letter-spacing:2px;">
          Create Account
        </button>
      </div>

      <div class="login-form-panel" id="login-panel-reset">
        <span class="login-forgot" style="text-align:left; display:inline-block; margin-bottom:20px;" onclick="loginShowReset(false)">← Back to sign in</span>
        <p style="color:#d4b87e; font-size:1.1em; letter-spacing:2px; margin-bottom:10px;">Reset Password</p>
        <p style="color:#8b7355; font-size:0.88em; line-height:1.7; margin-bottom:24px; font-style:italic;">
          Enter your email and we'll send you a reset link.
        </p>
        <div class="login-global-error" id="reset-global-error"></div>
        <div class="login-global-success" id="reset-global-success"></div>
        <div class="login-field" style="margin-bottom:28px;">
          <label>Email</label>
          <input type="email" id="reset-email" placeholder="your@email.com" />
          <div class="login-field-error" id="reset-email-err">Please enter a valid email address.</div>
        </div>
        <button class="btn-guess" id="reset-btn" onclick="handleReset()" style="width:100%; padding:15px; font-size:14px; letter-spacing:2px;">
          Send Reset Link
        </button>
      </div>

      <p style="text-align:center; margin-top:20px;">
        <button onclick="continueAsGuest()" class="btn-hint" style="width:100%; padding:15px; font-size:14px; letter-spacing:2px; margin-top:12px;">
          Play Without Account
        </button>
      </p>
    </div>
  `;

  document.addEventListener('keydown', loginEnterHandler);
}

function showLoginModal() {
  const overlay = document.createElement('div');
  overlay.className = 'modal-overlay';
  overlay.id = 'login-modal-overlay';
  
  const box = document.createElement('div');
  box.className = 'modal-box';
  box.style.maxWidth = '480px';
  box.style.width = '90%';
  
  box.innerHTML = `
    <div class="tab-row" style="margin:-40px -40px 36px;">
      <button class="tab-btn active" id="tab-signin" onclick="loginSwitchTab('signin')">Sign In</button>
      <button class="tab-btn" id="tab-register" onclick="loginSwitchTab('register')">Create Account</button>
    </div>

    <div class="login-form-panel active" id="login-panel-signin">
      <div class="login-global-error" id="signin-global-error"></div>
      <div class="login-global-success" id="signin-global-success"></div>
      <div class="login-field">
        <label>Email</label>
        <input type="email" id="signin-email" placeholder="your@email.com" autocomplete="email" />
        <div class="login-field-error" id="signin-email-err">Please enter a valid email address.</div>
      </div>
      <div class="login-field">
        <label>Password</label>
        <input type="password" id="signin-password" placeholder="••••••••" autocomplete="current-password" />
        <div class="login-field-error" id="signin-password-err">Password is required.</div>
      </div>
      <span class="login-forgot" onclick="loginShowReset()">Forgot password?</span>
      <button class="btn-guess" id="signin-btn" onclick="handleSignInModal()" style="width:100%; padding:15px; font-size:14px; letter-spacing:2px;">
        Sign In
      </button>
    </div>

    <div class="login-form-panel" id="login-panel-register">
      <div class="login-global-error" id="register-global-error"></div>
      <div class="login-global-success" id="register-global-success"></div>
      <div class="login-field">
        <label>Email</label>
        <input type="email" id="reg-email" placeholder="your@email.com" autocomplete="email" />
        <div class="login-field-error" id="reg-email-err">Please enter a valid email address.</div>
      </div>
      <div class="login-field">
        <label>Password</label>
        <input type="password" id="reg-password" placeholder="At least 6 characters" autocomplete="new-password" />
        <div class="login-field-error" id="reg-password-err">Password must be at least 6 characters.</div>
      </div>
      <div class="login-field" style="margin-bottom:28px;">
        <label>Confirm Password</label>
        <input type="password" id="reg-confirm" placeholder="Repeat password" autocomplete="new-password" />
        <div class="login-field-error" id="reg-confirm-err">Passwords do not match.</div>
      </div>
      <button class="btn-guess" id="register-btn" onclick="handleRegisterModal()" style="width:100%; padding:15px; font-size:14px; letter-spacing:2px;">
        Create Account
      </button>
    </div>

    <div class="login-form-panel" id="login-panel-reset">
      <span class="login-forgot" style="display:inline-block; margin-bottom:20px;" onclick="loginShowReset(false)">← Back</span>
      <div class="login-global-error" id="reset-global-error"></div>
      <div class="login-global-success" id="reset-global-success"></div>
      <div class="login-field" style="margin-bottom:28px;">
        <label>Email</label>
        <input type="email" id="reset-email" placeholder="your@email.com" />
        <div class="login-field-error" id="reset-email-err">Please enter a valid email address.</div>
      </div>
      <button class="btn-guess" id="reset-btn" onclick="handleReset()" style="width:100%; padding:15px; font-size:14px; letter-spacing:2px;">
        Send Reset Link
      </button>
    </div>

    <button onclick="closeLoginModal()" class="btn-hint" style="width:100%; padding:12px; font-size:12px; letter-spacing:2px; margin-top:20px;">
      Continue Without Account
    </button>
  `;

  overlay.appendChild(box);
  document.body.appendChild(overlay);

  overlay.addEventListener('click', e => {
    if (e.target === overlay) closeLoginModal();
  });
}

function closeLoginModal() {
  const overlay = document.getElementById('login-modal-overlay');
  if (overlay) document.body.removeChild(overlay);
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
  await initUserStatsRow();

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
  await initUserStatsRow();

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
  showDifficultySelection();
}

async function logout() {
  const confirm = await customConfirm(
    'Confirm Logout',
    'Are you sure you want to end your research session?',
    'Logout',
    'Cancel'
  );

  if (confirm === 'true') {
    await sb.auth.signOut();
    localStorage.removeItem('phylosaur-discoveries');
    currentUser = null;
    currentUserId = null;
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
      <button class="btn-guess" id="update-btn" onclick="handlePasswordUpdate()" style="width:100%; padding:15px; font-size:14px; letter-spacing:2px;">
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