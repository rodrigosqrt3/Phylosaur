// ═══════════════════════════════════════════════
// USER ACCOUNT SYSTEM
// ═══════════════════════════════════════════════
let accountRequestGeneration = 0;
let authOperationQueue = Promise.resolve();

function queueAuthOperation(operation) {
  const pending = authOperationQueue.then(operation);
  authOperationQueue = pending.catch(() => {});
  return pending;
}

async function runAccountFormRequest(buttonId, panel, operation) {
  const button = document.getElementById(buttonId);
  if (!button || button.disabled) return;
  const generation = ++accountRequestGeneration;
  let ownerId = currentUserId;
  const request = {
    isCurrent: () => generation === accountRequestGeneration && currentUserId === ownerId
      && document.getElementById(buttonId) === button,
    adoptOwner(id) { ownerId = id; },
  };
  loginSetLoading(buttonId, true);
  try {
    await queueAuthOperation(async () => {
      if (request.isCurrent()) await operation(request);
    });
  } catch (error) {
    if (request.isCurrent()) loginShowGlobalError(panel, t("auth.requestFailed"));
    console.warn("Account operation could not be completed:", error?.name || "Error");
  } finally {
    // Release the original control only, never a replacement dialog with the same ID.
    button.disabled = false;
    button.classList.remove("btn-loading");
  }
}

async function discardCancelledAuthSession(data) {
  if (!data?.session?.access_token) return;
  const { data: current } = await sb.auth.getSession();
  if (current?.session?.access_token !== data.session.access_token) return;
  const { error } = await sb.auth.signOut({ scope: "local" });
  if (error) throw error;
}

function clearAccountProgressState() {
  userStats = {
    gamesPlayed: 0, gamesWon: 0, totalGuesses: 0, bestScore: null,
    difficultyStats: Object.fromEntries(["muito_facil", "facil", "normal", "dificil", "muito_dificil"]
      .map(difficulty => [difficulty, { played: 0, won: 0, avgGuesses: 0 }])),
    recentGames: [], achievements: [],
  };
  currentAccountProgress = null;
  if (typeof museumDiscoveryRecords !== "undefined") museumDiscoveryRecords = {};
}

async function initializeUserSystem() {
  const generation = ++accountRequestGeneration;
  const previousOwnerId = currentUserId;
  const isCurrent = () => generation === accountRequestGeneration && currentUserId === previousOwnerId;
  try {
    const { data: { session } } = await sb.auth.getSession();
    if (!session || !isCurrent()) return null;

    const ownerId = session.user.id;
    const [profileResult, statsResult] = await Promise.all([
      sb.from('profiles').select('username').eq('id', ownerId).single(),
      sb.from('statistics').select('*').eq('user_id', ownerId).single()
    ]);
    if (!isCurrent()) return null;
    const profile = profileResult.data;
    const stats = statsResult.data;

    if (currentUserId !== ownerId) clearAccountProgressState();
    currentUserId = ownerId;
    isAnalyticsAdmin = false;
    analyticsAccessChecked = false;
    currentUser = profile?.username || session.user.email?.split('@')[0] || null;

    if (stats) {
      userStats.gamesPlayed = stats.games_played;
      userStats.gamesWon = stats.games_won;
      userStats.totalGuesses = stats.total_guesses;
      userStats.bestScore = stats.best_score;
    }

    void claimGuestProgressOnLogin({ showNotice: false })
      .then(() => {
        if (generation === accountRequestGeneration && currentUserId === ownerId
            && typeof getCurrentAppRoute === 'function' && getCurrentAppRoute() === '/') {
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

async function initUserStatsRow(ownerId = currentUserId, isCurrent = () => currentUserId === ownerId) {
  if (!ownerId || !isCurrent()) return;

  const { data: current } = await sb.from('statistics')
    .select('user_id')
    .eq('user_id', ownerId)
    .single();

  if (current || !isCurrent()) return;

  const { error } = await sb.from('statistics').insert({
    user_id:       ownerId,
    games_played:  0,
    games_won:     0,
    total_guesses: 0,
    best_score:    null,
    updated_at:    new Date().toISOString()
  });
  if (error) throw error;
}

function showLoginScreen() {
  setHeaderControls('login');
  const appContent = document.getElementById('app-content');

  appContent.innerHTML = `
    <div class="game-card login-card">
      <div class="tab-row login-tab-row" role="tablist" aria-label="${t('auth.accountAccess')}">
        <button class="tab-btn active" id="tab-signin" role="tab" aria-selected="true"
                aria-controls="login-panel-signin" onclick="loginSwitchTab('signin')">${t('auth.signIn')}</button>
        <button class="tab-btn" id="tab-register" role="tab" aria-selected="false"
                aria-controls="login-panel-register" onclick="loginSwitchTab('register')">${t('auth.createAccount')}</button>
      </div>

      <div class="login-form-panel active" id="login-panel-signin" role="tabpanel" aria-labelledby="tab-signin">
        <div class="login-global-error" id="signin-global-error"></div>
        <div class="login-global-success" id="signin-global-success"></div>
        <div class="login-field">
          <label for="signin-email">${t('auth.email')}</label>
          <input type="email" id="signin-email" placeholder="${t('auth.emailPlaceholder')}" autocomplete="email" />
          <div class="login-field-error" id="signin-email-err">${t('auth.validEmail')}</div>
        </div>
        <div class="login-field">
          <label for="signin-password">${t('auth.password')}</label>
          <input type="password" id="signin-password" placeholder="••••••••" autocomplete="current-password" />
          <div class="login-field-error" id="signin-password-err">${t('auth.passwordRequired')}</div>
        </div>
        <button type="button" class="login-forgot login-text-action" onclick="loginShowReset()">${t('auth.forgot')}</button>
        <button class="btn-guess btn-block btn-large" id="signin-btn" onclick="handleSignIn()">
          ${t('auth.signIn')}
        </button>
      </div>

      <div class="login-form-panel" id="login-panel-register" role="tabpanel" aria-labelledby="tab-register">
        <div class="login-global-error" id="register-global-error"></div>
        <div class="login-global-success" id="register-global-success"></div>
        <div class="login-field">
          <label for="reg-email">${t('auth.email')}</label>
          <input type="email" id="reg-email" placeholder="${t('auth.emailPlaceholder')}" autocomplete="email" />
          <div class="login-field-error" id="reg-email-err">${t('auth.validEmail')}</div>
        </div>
        <div class="login-field">
          <label for="reg-password">${t('auth.password')}</label>
          <input type="password" id="reg-password" placeholder="${t('auth.passwordPlaceholder')}" autocomplete="new-password" />
          <div class="login-field-error" id="reg-password-err">${t('auth.passwordLength')}</div>
        </div>
        <div class="login-field login-field-last">
          <label for="reg-confirm">${t('auth.confirmPassword')}</label>
          <input type="password" id="reg-confirm" placeholder="${t('auth.repeatPassword')}" autocomplete="new-password" />
          <div class="login-field-error" id="reg-confirm-err">${t('auth.passwordMismatch')}</div>
        </div>
        <button class="btn-guess btn-block btn-large" id="register-btn" onclick="handleRegister()">
          ${t('auth.createAccount')}
        </button>
      </div>

      <div class="login-form-panel" id="login-panel-reset">
        <button type="button" class="login-text-action login-back-link" onclick="loginShowReset(false)"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('auth.backSignIn')}</span></button>
        <p class="login-reset-title">${t('auth.resetPassword')}</p>
        <p class="login-reset-copy">
          ${t('auth.resetCopy')}
        </p>
        <div class="login-global-error" id="reset-global-error"></div>
        <div class="login-global-success" id="reset-global-success"></div>
        <div class="login-field login-field-last">
          <label for="reset-email">${t('auth.email')}</label>
          <input type="email" id="reset-email" placeholder="${t('auth.emailPlaceholder')}" />
          <div class="login-field-error" id="reset-email-err">${t('auth.validEmail')}</div>
        </div>
        <button class="btn-guess btn-block btn-large" id="reset-btn" onclick="handleReset()">
          ${t('auth.sendReset')}
        </button>
      </div>

      <p class="login-guest-action">
        <button onclick="continueAsGuest()" class="btn-hint btn-block btn-large btn-spaced">
          ${t('auth.playGuest')}
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
  overlay.setAttribute('aria-label', t('auth.accountAccess'));
  
  const box = document.createElement('div');
  box.className = 'modal-box login-modal-box';
  box.tabIndex = -1;
  
  box.innerHTML = `
    <div class="tab-row login-tab-row" role="tablist" aria-label="${t('auth.accountAccess')}">
      <button class="tab-btn active" id="tab-signin" role="tab" aria-selected="true"
              aria-controls="login-panel-signin" onclick="loginSwitchTab('signin')">${t('auth.signIn')}</button>
      <button class="tab-btn" id="tab-register" role="tab" aria-selected="false"
              aria-controls="login-panel-register" onclick="loginSwitchTab('register')">${t('auth.createAccount')}</button>
    </div>

    <div class="login-form-panel active" id="login-panel-signin" role="tabpanel" aria-labelledby="tab-signin">
      <div class="login-global-error" id="signin-global-error"></div>
      <div class="login-global-success" id="signin-global-success"></div>
      <div class="login-field">
        <label for="signin-email">${t('auth.email')}</label>
        <input type="email" id="signin-email" placeholder="${t('auth.emailPlaceholder')}" autocomplete="email" />
        <div class="login-field-error" id="signin-email-err">${t('auth.validEmail')}</div>
      </div>
      <div class="login-field">
        <label for="signin-password">${t('auth.password')}</label>
        <input type="password" id="signin-password" placeholder="••••••••" autocomplete="current-password" />
        <div class="login-field-error" id="signin-password-err">${t('auth.passwordRequired')}</div>
      </div>
      <button type="button" class="login-forgot login-text-action" onclick="loginShowReset()">${t('auth.forgot')}</button>
      <button class="btn-guess btn-block btn-large" id="signin-btn" onclick="handleSignInModal()">
        ${t('auth.signIn')}
      </button>
    </div>

    <div class="login-form-panel" id="login-panel-register" role="tabpanel" aria-labelledby="tab-register">
      <div class="login-global-error" id="register-global-error"></div>
      <div class="login-global-success" id="register-global-success"></div>
      <div class="login-field">
        <label for="reg-email">${t('auth.email')}</label>
        <input type="email" id="reg-email" placeholder="${t('auth.emailPlaceholder')}" autocomplete="email" />
        <div class="login-field-error" id="reg-email-err">${t('auth.validEmail')}</div>
      </div>
      <div class="login-field">
        <label for="reg-password">${t('auth.password')}</label>
        <input type="password" id="reg-password" placeholder="${t('auth.passwordPlaceholder')}" autocomplete="new-password" />
        <div class="login-field-error" id="reg-password-err">${t('auth.passwordLength')}</div>
      </div>
      <div class="login-field login-field-last">
        <label for="reg-confirm">${t('auth.confirmPassword')}</label>
        <input type="password" id="reg-confirm" placeholder="${t('auth.repeatPassword')}" autocomplete="new-password" />
        <div class="login-field-error" id="reg-confirm-err">${t('auth.passwordMismatch')}</div>
      </div>
      <button class="btn-guess btn-block btn-large" id="register-btn" onclick="handleRegisterModal()">
        ${t('auth.createAccount')}
      </button>
    </div>

    <div class="login-form-panel" id="login-panel-reset">
      <button type="button" class="login-text-action login-back-link" onclick="loginShowReset(false)"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('auth.backSignIn')}</span></button>
      <div class="login-global-error" id="reset-global-error"></div>
      <div class="login-global-success" id="reset-global-success"></div>
      <div class="login-field login-field-last">
        <label for="reset-email">${t('auth.email')}</label>
        <input type="email" id="reset-email" placeholder="${t('auth.emailPlaceholder')}" />
        <div class="login-field-error" id="reset-email-err">${t('auth.validEmail')}</div>
      </div>
      <button class="btn-guess btn-block btn-large" id="reset-btn" onclick="handleReset()">
        ${t('auth.sendReset')}
      </button>
    </div>

    <button onclick="closeLoginModal()" class="btn-hint btn-block btn-spaced">
      ${t('auth.continueGuest')}
    </button>
  `;

  overlay.appendChild(box);
  document.body.appendChild(overlay);
  document.body.style.overflow = 'hidden';

  const modalKeyHandler = event => {
    if (!isTopAppOverlay(overlay)) return;
    if (event.key === 'Escape') {
      event.preventDefault();
      event.stopImmediatePropagation();
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
  activeLoginModalCleanup = ({ restoreFocus = true } = {}) => {
    document.removeEventListener('keydown', modalKeyHandler, true);
    document.body.style.overflow = previousBodyOverflow;
    if (restoreFocus && previouslyFocused instanceof HTMLElement && previouslyFocused.isConnected) {
      previouslyFocused.focus();
    }
  };
  overlay.dismissAppOverlay = options => closeLoginModal(options);

  overlay.addEventListener('click', e => {
    if (e.target === overlay) closeLoginModal();
  });

  queueMicrotask(() => {
    if (isTopAppOverlay(overlay)) document.getElementById('signin-email')?.focus() || box.focus();
  });
}

function closeLoginModal({ restoreFocus = true } = {}) {
  if (document.getElementById("login-modal-overlay")) accountRequestGeneration++;
  const overlay = document.getElementById('login-modal-overlay');
  overlay?.remove();
  activeLoginModalCleanup?.({ restoreFocus });
  activeLoginModalCleanup = null;
}

async function handleSignInModal() {
  return handleAccountSignIn(true);
}

async function handleAccountSignIn(modal = false) {
  loginClearErrors();
  const email    = document.getElementById('signin-email')?.value.trim();
  const password = document.getElementById('signin-password')?.value;
  let valid = true;

  if (!loginIsValidEmail(email))  { loginShowFieldError('signin-email-err');    valid = false; }
  if (!password)                  { loginShowFieldError('signin-password-err'); valid = false; }
  if (!valid) return;

  return runAccountFormRequest("signin-btn", "signin", async request => {
    let authenticated = null;
    let adopted = false;
    try {
      const { data, error } = await sb.auth.signInWithPassword({ email, password });
      authenticated = data;
      if (!request.isCurrent()) return;
      if (error) {
        const message = error.code === "invalid_credentials" || error.message?.includes("Invalid login credentials")
          ? t("auth.invalidCredentials")
          : error.status >= 500 || error.name === "AuthRetryableFetchError"
            ? t("auth.requestFailed") : error.message || t("auth.requestFailed");
        loginShowGlobalError("signin", message);
        return;
      }
      if (!data?.user?.id) throw new Error("Missing authenticated user");
      const ownerId = data.user.id;
      let profile = null;
      try {
        const result = await sb.from("profiles").select("username").eq("id", ownerId).single();
        profile = result.data;
      } catch {
        console.warn("Account profile unavailable; using the account name fallback.");
      }
      if (!request.isCurrent()) return;
      if (currentUserId !== ownerId) clearAccountProgressState();
      currentUser = profile?.username || email.split("@")[0];
      currentUserId = ownerId;
      request.adoptOwner(ownerId);
      adopted = true;
      isAnalyticsAdmin = false;
      analyticsAccessChecked = false;
      try {
        await initUserStatsRow(ownerId, request.isCurrent);
        if (!request.isCurrent()) return;
        await claimGuestProgressOnLogin({ showNotice: true });
      } catch (syncError) {
        // Authentication already succeeded; optional progress sync can retry.
        console.warn("Account progress synchronization will need a retry:", syncError?.name || "Error");
      }
      if (!request.isCurrent()) return;
      if (modal) {
        closeLoginModal();
        setHeaderControls(selectedDifficulty ? "game" : "difficulty");
      } else {
        document.removeEventListener("keydown", loginEnterHandler);
        showDifficultySelection();
      }
    } finally {
      // The auth SDK cannot cancel a request already sent. Dispose only its
      // own session, while queued newer auth operations are still waiting.
      if (!adopted && authenticated && !request.isCurrent()) {
        await discardCancelledAuthSession(authenticated);
      }
    }
  });
}

async function handleRegisterModal() {
  return handleAccountRegister();
}

async function handleAccountRegister() {
  loginClearErrors();
  const email    = document.getElementById('reg-email')?.value.trim();
  const password = document.getElementById('reg-password')?.value || "";
  const confirm  = document.getElementById('reg-confirm')?.value || "";
  let valid = true;

  if (!loginIsValidEmail(email))   { loginShowFieldError('reg-email-err');    valid = false; }
  if (password.length < 6)         { loginShowFieldError('reg-password-err'); valid = false; }
  if (password !== confirm)        { loginShowFieldError('reg-confirm-err');  valid = false; }
  if (!valid) return;

  return runAccountFormRequest("register-btn", "register", async request => {
    let authenticated = null;
    try {
      const { data, error } = await sb.auth.signUp({ email, password });
      authenticated = data;
      if (!request.isCurrent()) return;
      if (error) {
        loginShowGlobalError("register", error.message);
        return;
      }
      if (data?.user) {
        await sb.from("profiles").insert({ id: data.user.id, username: email.split("@")[0] });
        if (!request.isCurrent()) return;
      }
      loginShowGlobalSuccess("register", t("auth.accountCreated"));
    } finally {
      if (authenticated?.session) {
        // Registration keeps its existing confirmation/sign-in workflow, even
        // on servers configured to auto-create a session on signup.
        await discardCancelledAuthSession(authenticated);
      }
    }
  });
}

function loginSwitchTab(tab) {
  accountRequestGeneration++;
  document.querySelectorAll('.login-tab-row .tab-btn').forEach(button => {
    const active = button.id === 'tab-' + tab;
    button.classList.toggle('active', active);
    button.setAttribute('aria-selected', String(active));
  });
  document.querySelectorAll('.login-form-panel').forEach(p => p.classList.remove('active'));
  document.getElementById('login-panel-' + tab).classList.add('active');
  loginClearErrors();
}

function loginShowReset(show = true) {
  accountRequestGeneration++;
  document.querySelectorAll('.login-form-panel').forEach(p => p.classList.remove('active'));
  document.querySelector('.login-tab-row').style.display = show ? 'none' : 'grid';
  if (show) {
    document.getElementById('login-panel-reset').classList.add('active');
  } else {
    document.getElementById('login-panel-signin').classList.add('active');
    document.querySelector('.login-tab-row').style.display = 'grid';
    document.querySelectorAll('.login-tab-row .tab-btn').forEach(button => {
      const active = button.id === 'tab-signin';
      button.classList.toggle('active', active);
      button.setAttribute('aria-selected', String(active));
    });
  }
}

function loginClearErrors() {
  document.querySelectorAll('.login-field-error').forEach(e => e.classList.remove('visible'));
  document.querySelectorAll('.login-field input').forEach(i => {
    i.classList.remove('input-error');
    i.removeAttribute('aria-invalid');
  });
  document.querySelectorAll('.login-global-error, .login-global-success').forEach(e => e.classList.remove('visible'));
}

function loginShowFieldError(errId) {
  const err = document.getElementById(errId);
  if (!err) return;
  err.classList.add('visible');
  err.previousElementSibling.classList.add('input-error');
  err.previousElementSibling.setAttribute('aria-invalid', 'true');
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
  return handleAccountSignIn(false);
}

async function handleRegister() {
  return handleAccountRegister();
}

async function handleReset() {
  loginClearErrors();
  const email = document.getElementById('reset-email')?.value.trim();

  if (!loginIsValidEmail(email)) { loginShowFieldError('reset-email-err'); return; }

  return runAccountFormRequest("reset-btn", "reset", async request => {
    const { error } = await sb.auth.resetPasswordForEmail(email, { redirectTo: window.location.origin });
    if (!request.isCurrent()) return;
    if (error) loginShowGlobalError("reset", error.message);
    else loginShowGlobalSuccess("reset", t("auth.resetSent"));
  });
}

async function continueAsGuest() {
  const generation = ++accountRequestGeneration;
  try {
    const { error } = await queueAuthOperation(() => sb.auth.signOut({ scope: "local" }));
    if (generation !== accountRequestGeneration) return;
    if (error) throw error;
  } catch (error) {
    if (generation === accountRequestGeneration) await customAlert(t("auth.accountAccess"), t("auth.requestFailed"));
    return;
  }
  currentUser = null;
  currentUserId = null;
  clearAccountProgressState();
  isAnalyticsAdmin = false;
  analyticsAccessChecked = true;
  if (typeof museumDiscoveryRecords !== 'undefined') museumDiscoveryRecords = {};
  showDifficultySelection();
}

async function logout() {
  const ownerId = currentUserId;
  const generation = accountRequestGeneration;
  const confirm = await customConfirm(
    t('auth.confirmLogout'),
    t('auth.confirmLogoutCopy'),
    t('auth.logout'),
    t('common.cancel')
  );

  if (confirm === 'true' && currentUserId === ownerId && generation === accountRequestGeneration) {
    const logoutGeneration = ++accountRequestGeneration;
    try {
      const { error } = await queueAuthOperation(() => sb.auth.signOut({ scope: "local" }));
      if (logoutGeneration !== accountRequestGeneration || currentUserId !== ownerId) return;
      if (error) throw error;
    } catch (error) {
      if (logoutGeneration === accountRequestGeneration && currentUserId === ownerId) {
        await customAlert(t("auth.accountAccess"), t("auth.requestFailed"));
      }
      return;
    }
    currentUser = null;
    currentUserId = null;
    isAnalyticsAdmin = false;
    analyticsAccessChecked = true;
    if (typeof museumDiscoveryRecords !== 'undefined') museumDiscoveryRecords = {};
    clearAccountProgressState();
    showDifficultySelection();
  }
}

function showPasswordUpdateForm() {
  setHeaderControls('login');
  const appContent = document.getElementById('app-content');

  appContent.innerHTML = `
    <div class="game-card login-card password-update-card">
      <h2 class="screen-title password-update-title">${t('auth.setPassword')}</h2>
      <p class="login-reset-copy password-update-copy">
        ${t('auth.choosePassword')}
      </p>
      <div class="login-global-error" id="update-global-error" role="alert"></div>
      <div class="login-global-success" id="update-global-success" role="status" aria-live="polite"></div>
      <div class="login-field">
        <label for="update-password">${t('auth.newPassword')}</label>
        <input type="password" id="update-password" placeholder="${t('auth.passwordPlaceholder')}"
               autocomplete="new-password" aria-describedby="update-password-err" />
        <div class="login-field-error" id="update-password-err">${t('auth.passwordLength')}</div>
      </div>
      <div class="login-field login-field-last">
        <label for="update-confirm">${t('auth.confirmPassword')}</label>
        <input type="password" id="update-confirm" placeholder="${t('auth.repeatPassword')}"
               autocomplete="new-password" aria-describedby="update-confirm-err" />
        <div class="login-field-error" id="update-confirm-err">${t('auth.passwordMismatch')}</div>
      </div>
      <button class="btn-guess btn-block btn-large" id="update-btn" onclick="handlePasswordUpdate()">
        ${t('auth.updatePassword')}
      </button>
    </div>
  `;

  focusAppScreenHeading('.password-update-title');
}

async function handlePasswordUpdate() {
  loginClearErrors();
  const password = document.getElementById('update-password')?.value || '';
  const confirm  = document.getElementById('update-confirm')?.value || '';
  let valid = true;

  if (password.length < 6) { loginShowFieldError('update-password-err'); valid = false; }
  if (password !== confirm) { loginShowFieldError('update-confirm-err');  valid = false; }
  if (!valid) return;

  return runAccountFormRequest("update-btn", "update", async request => {
    const { error } = await sb.auth.updateUser({ password });
    if (!request.isCurrent()) return;
    if (error) {
      loginShowGlobalError("update", error.message);
      return;
    }
    loginShowGlobalSuccess("update", t("auth.passwordUpdated"));
    window.history.replaceState({}, document.title, window.location.pathname);
    setTimeout(() => { if (request.isCurrent()) showDifficultySelection(); }, 1500);
  });
}