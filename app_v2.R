library(shiny)
library(shinyjs)

# ── Database syntax rules ─────────────────────────────────────────────────────
# type is now a list of flags: tiab, mesh, exp, plain (any combination)
# format_term emits one string per active flag, joined with OR

db_rules <- list(
  pubmed = list(
    name="PubMed",
    tiab_fmt  = function(t,p=NA) if(!is.na(p)) paste0('"',t,'"[tiab:~',p,']') else paste0('"',t,'"[tiab]'),
    mesh_fmt  = function(t,p=NA) paste0('"',t,'"[MeSH]'),
    exp_fmt   = function(t,p=NA) paste0('"',t,'"[MeSH]'),   # PubMed: MeSH = exp
    plain_fmt = function(t,p=NA) t,
    open="(", close=")", and=" AND ", or=" OR ", wrap=function(s) paste0("(",s,")")
  ),
  wos = list(
    name="Web of Science",
    tiab_fmt  = function(t,p=NA) if(grepl(" ",t)) paste0('"',t,'"') else t,
    mesh_fmt  = function(t,p=NA) if(grepl(" ",t)) paste0('"',t,'"') else t,
    exp_fmt   = function(t,p=NA) if(grepl(" ",t)) paste0('"',t,'"') else t,
    plain_fmt = function(t,p=NA) if(grepl(" ",t)) paste0('"',t,'"') else t,
    open="(", close=")", and=" AND ", or=" OR ", field=" (Topic)", wrap=function(s) s
  ),
  embase = list(
    name="Embase",
    tiab_fmt  = function(t,p=NA) if(!is.na(p)) paste0("'",t,"':ti,ab,kw") else paste0(t,":ti,ab,kw"),
    mesh_fmt  = function(t,p=NA) paste0("'",t,"'/mh"),
    exp_fmt   = function(t,p=NA) paste0("'",t,"'/exp"),
    plain_fmt = function(t,p=NA) paste0("'",t,"'"),
    open="(", close=")", and=" AND ", or=" OR ", wrap=function(s) s
  ),
  scopus = list(
    name="Scopus",
    tiab_fmt  = function(t,p=NA) if(grepl(" ",t)) paste0('"',t,'"') else t,
    mesh_fmt  = function(t,p=NA) if(grepl(" ",t)) paste0('"',t,'"') else t,
    exp_fmt   = function(t,p=NA) if(grepl(" ",t)) paste0('"',t,'"') else t,
    plain_fmt = function(t,p=NA) if(grepl(" ",t)) paste0('"',t,'"') else t,
    open="TITLE-ABS-KEY ( ", close=" )", and=" AND ", or=" OR ", wrap=function(s) paste0("( ",s," )")
  )
)

format_term <- function(tm, db) {
  r    <- db_rules[[db]]
  term <- trimws(tm$text)
  prox <- tm$prox
  flags <- tm$flags   # character vector e.g. c("tiab","mesh")
  if (!length(flags)) flags <- "tiab"
  
  near_wrap <- function(t, op) {
    if (!is.na(prox) && grepl(" ",t)) {
      w <- strsplit(t," ")[[1]]
      paste0('"',paste(w,collapse=paste0(" ",op,prox," ")),'"')
    } else if (grepl(" ",t)) paste0('"',t,'"') else t
  }
  
  # For WoS / Scopus proximity is embedded in the term itself; flags collapse to one
  if (db=="wos") {
    base <- near_wrap(term,"NEAR/")
    return(base)
  }
  if (db=="scopus") {
    base <- near_wrap(term,"W/")
    return(base)
  }
  
  # For Embase proximity is per-tiab flag; mesh/exp use controlled term forms
  parts <- character(0)
  if ("tiab"  %in% flags) {
    if (!is.na(prox) && grepl(" ",term)) {
      w <- strsplit(term," ")[[1]]
      parts <- c(parts, paste0("'",paste(w,collapse=paste0(" near/",prox," ")),"':ti,ab,kw"))
    } else {
      parts <- c(parts, r$tiab_fmt(term,prox))
    }
  }
  if ("mesh"  %in% flags) parts <- c(parts, r$mesh_fmt(term,prox))
  if ("exp"   %in% flags) parts <- c(parts, r$exp_fmt(term,prox))
  if ("plain" %in% flags) parts <- c(parts, r$plain_fmt(term,prox))
  
  if (db=="embase") {
    if (length(parts)==1) return(parts)
    return(paste0("(",paste(parts,collapse=" OR "),")"))
  }
  
  # PubMed: same logic, emit unique results
  parts <- character(0)
  if ("tiab"  %in% flags) parts <- c(parts, r$tiab_fmt(term,prox))
  if ("mesh"  %in% flags) parts <- c(parts, r$mesh_fmt(term,prox))
  if ("exp"   %in% flags) parts <- c(parts, r$exp_fmt(term,prox))
  if ("plain" %in% flags) parts <- c(parts, r$plain_fmt(term,prox))
  parts <- unique(parts)
  if (length(parts)==1) return(parts)
  paste0("(",paste(parts,collapse=" OR "),")")
}

build_query <- function(concepts, db) {
  r    <- db_rules[[db]]
  cons <- Filter(function(c) isTRUE(c$included), concepts)
  if (!length(cons)) return("— no concepts included in query —")
  strs <- vapply(cons, function(con) {
    if (!length(con$terms)) return("")
    fmt   <- vapply(con$terms, function(t) format_term(t,db), character(1))
    inner <- paste(fmt, collapse=r$or)
    if (db=="scopus")   paste0(r$open,inner,r$close)
    else if (db=="wos") paste0(r$open,inner,r$close,r$field)
    else                paste0(r$open,inner,r$close)
  }, character(1))
  strs <- strs[nzchar(strs)]
  if (!length(strs)) return("— add terms to included concepts —")
  r$wrap(paste(strs,collapse=r$and))
}

# ── CSS ───────────────────────────────────────────────────────────────────────
app_css <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@300;400;600&display=swap');
*,*::before,*::after{box-sizing:border-box;}

:root {
  --bg-base:#0f1117;--bg-panel:#13182a;--bg-card:#1a2236;--bg-card-act:#1d2a40;
  --bg-input:#111827;--bg-output:#060810;--bg-outpanel:#0a0d14;
  --bg-header:linear-gradient(135deg,#1a1f2e 0%,#12192b 100%);
  --bd-base:#1e2d45;--bd-panel:#2d3a52;--bd-card:#253450;--bd-card-act:#5b9bd5;
  --bd-input:#1e2d45;--bd-foc:#3a6a9a;--bd-output:#1a2535;
  --tx-head:#7eb8f7;--tx-pri:#e2e8f0;--tx-sec:#93b8d8;--tx-mut:#4a6a8a;
  --tx-faint:#2d4a6a;--tx-mono:#d1e4f5;--tx-query:#7eb8f7;--tx-lbl:#4a7ab5;
  --tx-cnt:#4a6a8a;--tx-prev:#3a5a7a;--tx-sub:#4a6080;
  --tog-off:#1e3a50;--tog-on:#1e5a38;--dot-off:#4a7a9a;--dot-on:#5dba6a;
  --badge-on-bg:#1a3a28;--badge-on-tx:#5dba6a;--badge-on-bd:#2a5a38;
  --badge-off-bg:#2a1a1a;--badge-off-tx:#a05050;--badge-off-bd:#4a2a2a;
  --tab-bg:#111827;--tab-act-bg:#1d3a5a;--tab-act-bd:#3a7ab5;--tab-act-tx:#7eb8f7;--tab-tx:#4a7ab5;--tab-hov:#131c2e;
  --btn-bg:#1d3a5a;--btn-bd:#2d5a8a;--btn-tx:#7eb8f7;--btn-hov:#254a72;
  --copy-bg:#1a3a5a;--copy-bd:#2a5a8a;--copy-tx:#7eb8f7;--copy-hov:#254a72;
  --copy-ok-bg:#1a4a2a;--copy-ok-bd:#2a7a3a;--copy-ok-tx:#5dba6a;
  --save-bd:#2a5a3a;--save-tx:#5dba6a;--save-hov:#1a3a28;
  --del-tx:#3a4a5a;--del-hov:#e05555;--delc-tx:#2a3a4a;
  --addc-bd:#2d4a6a;--addc-tx:#4a7ab5;--addc-hov-bg:#1a2a40;--addc-hov-bd:#4a7ab5;--addc-hov-tx:#7eb8f7;
  --sum-tx:#4a6a8a;--sum-hl:#5dba6a;
  --io-bg:#141c2e;--io-bd:#253450;--th-bg:#0f1117;--tr-hov:#131824;--scr:#1e2d45;
  --chip-tiab-bg:#1a3050;--chip-tiab-tx:#7eb8f7;--chip-tiab-bd:#2a5080;
  --chip-mesh-bg:#1a3a28;--chip-mesh-tx:#5dba6a;--chip-mesh-bd:#2a5a38;
  --chip-exp-bg:#1a2a3a;--chip-exp-tx:#c87af7;--chip-exp-bd:#4a2a6a;
  --chip-plain-bg:#2a2a1a;--chip-plain-tx:#c8a84a;--chip-plain-bd:#4a4a2a;
  --chip-off-bg:#111827;--chip-off-tx:#3a5a7a;--chip-off-bd:#1e2d45;
  --prox-bg:#2a1a3a;--prox-tx:#c87af7;--prox-bd:#4a2a6a;
  --bucket-bg:#111827;--bucket-bd:#1e2d45;--bucket-item-bg:#1a2236;
  --bucket-item-hov:#1d3050;--bucket-sel-bg:#1d3a5a;--bucket-sel-bd:#3a7ab5;
}
html[data-theme='light'] {
  --bg-base:#f4f6fb;--bg-panel:#eaeef7;--bg-card:#ffffff;--bg-card-act:#dde8f8;
  --bg-input:#ffffff;--bg-output:#f0f4fc;--bg-outpanel:#e8eef8;
  --bg-header:linear-gradient(135deg,#2a4a8a 0%,#1a3a7a 100%);
  --bd-base:#c5d4e8;--bd-panel:#b8cce0;--bd-card:#c8d8ec;--bd-card-act:#2a6abf;
  --bd-input:#bccce0;--bd-foc:#2a6abf;--bd-output:#b8cce0;
  --tx-head:#ffffff;--tx-pri:#1a2a40;--tx-sec:#1e4080;--tx-mut:#3a6090;
  --tx-faint:#6a90b8;--tx-mono:#1a3060;--tx-query:#1a4080;--tx-lbl:#2a5a9a;
  --tx-cnt:#5a7a9a;--tx-prev:#4a6a90;--tx-sub:#c8daf0;
  --tog-off:#b8cce0;--tog-on:#2a7a48;--dot-off:#6a90b0;--dot-on:#ffffff;
  --badge-on-bg:#d4edd8;--badge-on-tx:#1a6a30;--badge-on-bd:#6ac080;
  --badge-off-bg:#f0d8d8;--badge-off-tx:#902020;--badge-off-bd:#d08080;
  --tab-bg:#dde8f8;--tab-act-bg:#2a5aaa;--tab-act-bd:#1a4a9a;--tab-act-tx:#ffffff;--tab-tx:#2a5a9a;--tab-hov:#ccdaee;
  --btn-bg:#2a5aaa;--btn-bd:#1a4a9a;--btn-tx:#ffffff;--btn-hov:#1a4a9a;
  --copy-bg:#2a5aaa;--copy-bd:#1a4a9a;--copy-tx:#ffffff;--copy-hov:#1a4a9a;
  --copy-ok-bg:#2a7a48;--copy-ok-bd:#1a5a38;--copy-ok-tx:#ffffff;
  --save-bd:#1a6a30;--save-tx:#1a6a30;--save-hov:#d4edd8;
  --del-tx:#8aaac8;--del-hov:#cc2222;--delc-tx:#8aaac8;
  --addc-bd:#9ab8d8;--addc-tx:#2a5a9a;--addc-hov-bg:#ccdaee;--addc-hov-bd:#2a5a9a;--addc-hov-tx:#1a3a7a;
  --sum-tx:#4a6a8a;--sum-hl:#1a6a30;
  --io-bg:rgba(255,255,255,0.15);--io-bd:rgba(255,255,255,0.35);--th-bg:#f4f6fb;--tr-hov:#eaf0fa;--scr:#9ab8d8;
  --chip-tiab-bg:#ddeeff;--chip-tiab-tx:#1a4a9a;--chip-tiab-bd:#9ab8e8;
  --chip-mesh-bg:#d4edd8;--chip-mesh-tx:#1a6a30;--chip-mesh-bd:#6ac080;
  --chip-exp-bg:#ede8f8;--chip-exp-tx:#6a30b0;--chip-exp-bd:#c8a8e8;
  --chip-plain-bg:#fff8dc;--chip-plain-tx:#7a6010;--chip-plain-bd:#c8a84a;
  --chip-off-bg:#e8eef7;--chip-off-tx:#8aaac8;--chip-off-bd:#c5d4e8;
  --prox-bg:#ede8f8;--prox-tx:#6a30b0;--prox-bd:#c8a8e8;
  --bucket-bg:#f0f4fc;--bucket-bd:#c5d4e8;--bucket-item-bg:#ffffff;
  --bucket-item-hov:#dde8f8;--bucket-sel-bg:#dde8f8;--bucket-sel-bd:#2a6abf;
}

body{background:var(--bg-base);color:var(--tx-pri);font-family:'IBM Plex Sans',sans-serif;margin:0;overflow:hidden;font-size:16px;}

/* Header */
.app-header{background:var(--bg-header);border-bottom:1px solid var(--bd-panel);padding:12px 20px;display:flex;align-items:center;justify-content:space-between;}
.app-header-left{display:flex;align-items:center;gap:12px;}
.app-header h1{margin:0;font-size:1.3rem;font-weight:600;color:var(--tx-head);letter-spacing:.03em;}
.app-header-sub{font-size:.82rem;color:var(--tx-sub);font-family:'IBM Plex Mono',monospace;}
.io-toolbar{display:flex;align-items:center;gap:7px;}
.io-btn{background:var(--io-bg);border:1px solid var(--io-bd);color:var(--tx-head);border-radius:6px;padding:6px 14px;font-size:.9rem;cursor:pointer;font-family:'IBM Plex Sans',sans-serif;transition:all .15s;display:flex;align-items:center;gap:5px;}
.io-btn:hover{background:rgba(255,255,255,0.22);}
.io-btn.save{border-color:var(--save-bd);color:var(--save-tx);}
html[data-theme='light'] .io-btn.save{color:#fff;}
.io-btn.save:hover{background:var(--save-hov);}
.theme-btn{background:var(--io-bg);border:1px solid var(--io-bd);color:var(--tx-head);border-radius:6px;padding:6px 10px;font-size:1rem;cursor:pointer;transition:all .15s;line-height:1;}
.theme-btn:hover{background:rgba(255,255,255,0.22);}
#load_file_input{display:none;}

/* Layout */
.main-layout{display:flex;height:calc(100vh - 57px);overflow:hidden;}

/* Left: concepts panel */
.concepts-panel{width:290px;min-width:240px;background:var(--bg-panel);border-right:1px solid var(--bd-base);display:flex;flex-direction:column;overflow:hidden;}
.panel-header{padding:10px 13px 8px;font-size:.75rem;font-weight:600;letter-spacing:.1em;text-transform:uppercase;color:var(--tx-lbl);border-bottom:1px solid var(--bd-base);display:flex;align-items:center;justify-content:space-between;flex-shrink:0;}
.concepts-list{flex:1;overflow-y:auto;padding:7px;}
.concept-card{background:var(--bg-card);border:1px solid var(--bd-card);border-radius:8px;margin-bottom:6px;overflow:hidden;transition:border-color .15s;}
.concept-card:hover{border-color:var(--bd-foc);}
.concept-card.active{border-color:var(--bd-card-act);background:var(--bg-card-act);}
.concept-card.excluded{opacity:.5;}
.concept-card-header{display:flex;align-items:center;gap:6px;padding:8px 10px;cursor:pointer;}
.concept-card-label{flex:1;font-size:.88rem;font-weight:600;color:var(--tx-sec);min-width:0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}
.concept-card-count{font-size:.73rem;color:var(--tx-cnt);font-family:'IBM Plex Mono',monospace;background:var(--bg-input);border-radius:10px;padding:2px 7px;flex-shrink:0;}
.include-toggle{width:34px;height:19px;border-radius:10px;border:none;cursor:pointer;position:relative;flex-shrink:0;transition:background .2s;background:var(--tog-off);}
.include-toggle.on{background:var(--tog-on);}
.include-toggle::after{content:'';position:absolute;top:2.5px;left:2.5px;width:14px;height:14px;border-radius:50%;background:var(--dot-off);transition:transform .2s,background .2s;}
.include-toggle.on::after{transform:translateX(15px);background:var(--dot-on);}
.concept-card-preview{padding:0 10px 8px;font-size:.74rem;color:var(--tx-prev);font-family:'IBM Plex Mono',monospace;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;cursor:pointer;}
.btn-del-concept{background:none;border:none;color:var(--delc-tx);cursor:pointer;font-size:.95rem;padding:1px 4px;border-radius:4px;transition:color .15s;flex-shrink:0;}
.btn-del-concept:hover{color:var(--del-hov);}
.btn-add-concept{margin:5px 7px 7px;background:transparent;border:1px dashed var(--addc-bd);color:var(--addc-tx);border-radius:7px;padding:8px;width:calc(100% - 14px);font-size:.88rem;cursor:pointer;transition:all .15s;}
.btn-add-concept:hover{background:var(--addc-hov-bg);border-color:var(--addc-hov-bd);color:var(--addc-hov-tx);}

/* Centre: editor */
.editor-panel{flex:1;display:flex;flex-direction:column;overflow:hidden;background:var(--bg-base);min-width:0;}
.editor-top{flex:1;display:flex;flex-direction:column;overflow:hidden;padding:14px 18px 8px;}
.concept-title-row{display:flex;align-items:center;gap:10px;margin-bottom:11px;flex-shrink:0;}
.concept-title-input{background:transparent;border:none;border-bottom:2px solid var(--bd-base);color:var(--tx-sec);font-size:1.15rem;font-weight:600;font-family:'IBM Plex Sans',sans-serif;outline:none;flex:1;padding-bottom:4px;transition:border-color .15s;}
.concept-title-input:focus{border-color:var(--bd-foc);}
.include-badge{font-size:.78rem;font-family:'IBM Plex Mono',monospace;border-radius:5px;padding:3px 8px;flex-shrink:0;}
.include-badge.on{background:var(--badge-on-bg);color:var(--badge-on-tx);border:1px solid var(--badge-on-bd);}
.include-badge.off{background:var(--badge-off-bg);color:var(--badge-off-tx);border:1px solid var(--badge-off-bd);}

/* Term list */
.terms-list-wrap{flex:1;overflow-y:auto;margin-bottom:4px;}
.term-row{display:grid;grid-template-columns:1fr auto auto auto;align-items:center;gap:6px;padding:5px 3px;border-bottom:1px solid var(--bd-base);}
.term-row:last-child{border-bottom:none;}
.term-row:hover{background:var(--tr-hov);}
.term-text-display{font-family:'IBM Plex Mono',monospace;font-size:.88rem;color:var(--tx-mono);padding:4px 6px;border-radius:4px;cursor:text;min-width:0;word-break:break-all;border:1px solid transparent;transition:border-color .15s;}
.term-text-display:hover{border-color:var(--bd-input);}
.term-text-edit{font-family:'IBM Plex Mono',monospace;font-size:.88rem;color:var(--tx-mono);background:var(--bg-input);border:1px solid var(--bd-foc);border-radius:4px;padding:4px 6px;outline:none;width:100%;}

/* Multi-flag chips */
.type-chips{display:flex;gap:3px;flex-shrink:0;}
.flag-chip{font-family:'IBM Plex Mono',monospace;font-size:.72rem;border-radius:10px;padding:2px 8px;cursor:pointer;border:1px solid;white-space:nowrap;transition:all .15s;opacity:.45;}
.flag-chip.on{opacity:1;}
.flag-chip[data-flag='tiab']{background:var(--chip-tiab-bg);color:var(--chip-tiab-tx);border-color:var(--chip-tiab-bd);}
.flag-chip[data-flag='mesh']{background:var(--chip-mesh-bg);color:var(--chip-mesh-tx);border-color:var(--chip-mesh-bd);}
.flag-chip[data-flag='exp'] {background:var(--chip-exp-bg); color:var(--chip-exp-tx); border-color:var(--chip-exp-bd);}
.flag-chip[data-flag='plain']{background:var(--chip-plain-bg);color:var(--chip-plain-tx);border-color:var(--chip-plain-bd);}
.flag-chip:hover{filter:brightness(1.15);opacity:1;}

/* Prox badge */
.prox-wrap{display:flex;align-items:center;flex-shrink:0;}
.prox-badge{font-family:'IBM Plex Mono',monospace;font-size:.72rem;background:var(--prox-bg);color:var(--prox-tx);border:1px solid var(--prox-bd);border-radius:10px;padding:2px 8px;cursor:pointer;white-space:nowrap;}
.prox-badge.empty{background:transparent;color:var(--tx-faint);border:1px dashed var(--bd-input);cursor:pointer;}
.prox-badge.empty:hover{border-color:var(--prox-bd);color:var(--prox-tx);}
.prox-input-inline{font-family:'IBM Plex Mono',monospace;font-size:.75rem;width:46px;text-align:center;background:var(--bg-input);border:1px solid var(--prox-bd);color:var(--prox-tx);border-radius:6px;padding:2px 4px;outline:none;}

.btn-del{background:none;border:none;color:var(--del-tx);cursor:pointer;font-size:1rem;padding:2px 5px;border-radius:4px;transition:color .15s;flex-shrink:0;}
.btn-del:hover{color:var(--del-hov);}

/* Add term row */
.add-term-row{display:flex;gap:7px;align-items:center;margin-top:8px;flex-shrink:0;}
.term-new-input{flex:1;background:var(--bg-input);border:1px solid var(--bd-input);color:var(--tx-mono);border-radius:6px;padding:6px 10px;font-size:.9rem;font-family:'IBM Plex Mono',monospace;outline:none;}
.term-new-input:focus{border-color:var(--bd-foc);}
.btn-add-term{background:var(--btn-bg);border:1px solid var(--btn-bd);color:var(--btn-tx);border-radius:6px;padding:6px 14px;font-size:.88rem;cursor:pointer;transition:all .15s;}
.btn-add-term:hover{background:var(--btn-hov);}

/* Output */
.output-panel{background:var(--bg-outpanel);border-top:1px solid var(--bd-base);padding:11px 18px 13px;flex-shrink:0;}
.output-top-row{display:flex;align-items:center;justify-content:space-between;margin-bottom:8px;}
.db-tabs{display:flex;gap:3px;}
.db-tab{background:var(--tab-bg);border:1px solid var(--bd-base);color:var(--tab-tx);border-radius:5px;padding:4px 11px;font-size:.82rem;cursor:pointer;font-family:'IBM Plex Mono',monospace;transition:all .15s;}
.db-tab.active{background:var(--tab-act-bg);border-color:var(--tab-act-bd);color:var(--tab-act-tx);}
.db-tab:hover:not(.active){background:var(--tab-hov);}
.included-summary{font-size:.78rem;color:var(--sum-tx);font-family:'IBM Plex Mono',monospace;}
.included-summary span{color:var(--sum-hl);}
.query-box{background:var(--bg-output);border:1px solid var(--bd-output);border-radius:7px;padding:10px 12px;font-family:'IBM Plex Mono',monospace;font-size:.82rem;color:var(--tx-query);white-space:pre-wrap;word-break:break-all;max-height:115px;overflow-y:auto;line-height:1.6;}
.copy-btn{margin-top:7px;background:var(--copy-bg);border:1px solid var(--copy-bd);color:var(--copy-tx);border-radius:6px;padding:5px 16px;font-size:.85rem;cursor:pointer;transition:all .15s;float:right;}
.copy-btn:hover{background:var(--copy-hov);}
.copy-btn.copied{background:var(--copy-ok-bg);border-color:var(--copy-ok-bd);color:var(--copy-ok-tx);}

/* Right: word bucket */
.bucket-panel{width:250px;min-width:210px;background:var(--bg-panel);border-left:1px solid var(--bd-base);display:flex;flex-direction:column;overflow:hidden;}
.bucket-header{padding:10px 13px 8px;font-size:.75rem;font-weight:600;letter-spacing:.1em;text-transform:uppercase;color:var(--tx-lbl);border-bottom:1px solid var(--bd-base);flex-shrink:0;display:flex;align-items:center;justify-content:space-between;}
.bucket-search{padding:7px 10px;border-bottom:1px solid var(--bd-base);flex-shrink:0;}
.bucket-search-input{width:100%;background:var(--bg-input);border:1px solid var(--bd-input);color:var(--tx-mono);border-radius:5px;padding:5px 8px;font-size:.82rem;font-family:'IBM Plex Mono',monospace;outline:none;}
.bucket-search-input:focus{border-color:var(--bd-foc);}
.bucket-list{flex:1;overflow-y:auto;padding:6px;}
.bucket-item{display:flex;align-items:center;gap:5px;padding:5px 8px;border-radius:6px;margin-bottom:3px;cursor:pointer;background:var(--bucket-item-bg);border:1px solid transparent;transition:all .15s;font-family:'IBM Plex Mono',monospace;font-size:.82rem;color:var(--tx-mono);}
.bucket-item:hover{background:var(--bucket-item-hov);border-color:var(--bd-foc);}
.bucket-item.selected{background:var(--bucket-sel-bg);border-color:var(--bucket-sel-bd);}
.bucket-item-text{flex:1;min-width:0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}
.bucket-item-del{background:none;border:none;color:var(--del-tx);cursor:pointer;font-size:.85rem;padding:0 2px;border-radius:3px;flex-shrink:0;opacity:0;transition:opacity .1s,color .15s;}
.bucket-item:hover .bucket-item-del{opacity:1;}
.bucket-item-del:hover{color:var(--del-hov);}
.bucket-add-row{padding:7px 10px;border-top:1px solid var(--bd-base);flex-shrink:0;display:flex;gap:5px;}
.bucket-new-input{flex:1;background:var(--bg-input);border:1px solid var(--bd-input);color:var(--tx-mono);border-radius:5px;padding:5px 7px;font-size:.82rem;font-family:'IBM Plex Mono',monospace;outline:none;}
.bucket-new-input:focus{border-color:var(--bd-foc);}
.bucket-add-btn{background:var(--btn-bg);border:1px solid var(--btn-bd);color:var(--btn-tx);border-radius:5px;padding:5px 9px;font-size:.82rem;cursor:pointer;transition:all .15s;white-space:nowrap;}
.bucket-add-btn:hover{background:var(--btn-hov);}
.bucket-use-btn{width:calc(100% - 20px);margin:0 10px 8px;background:var(--badge-on-bg);border:1px solid var(--badge-on-bd);color:var(--badge-on-tx);border-radius:6px;padding:6px;font-size:.82rem;cursor:pointer;transition:all .15s;}
.bucket-use-btn:hover{filter:brightness(1.1);}
.bucket-use-btn:disabled{opacity:.4;cursor:default;}
.bucket-count{font-size:.7rem;color:var(--tx-cnt);font-family:'IBM Plex Mono',monospace;background:var(--bg-input);border-radius:8px;padding:1px 6px;}

.empty-state{display:flex;flex-direction:column;align-items:center;justify-content:center;flex:1;color:var(--tx-faint);font-size:.95rem;gap:8px;padding:30px;text-align:center;}
.empty-state .icon{font-size:2.2rem;}
::-webkit-scrollbar{width:4px;height:4px;}
::-webkit-scrollbar-track{background:transparent;}
::-webkit-scrollbar-thumb{background:var(--scr);border-radius:3px;}
"

# ── JS ────────────────────────────────────────────────────────────────────────
term_js <- "
(function(){

var terms = [];
var conceptIdx = -1;

function pushTerms() {
  Shiny.setInputValue('terms_update', {ci: conceptIdx, terms: terms}, {priority:'event'});
}

var FLAG_DEFS = [
  {key:'tiab',  label:'tiab'},
  {key:'mesh',  label:'MeSH'},
  {key:'exp',   label:'exp'},
  {key:'plain', label:'plain'}
];

function proxLabel(p) { return (p===null||p===undefined||p==='') ? null : 'NEAR/'+p; }

function renderAll() {
  var wrap = document.getElementById('terms-list');
  if (!wrap) return;
  wrap.innerHTML = '';
  terms.forEach(function(tm,i){ wrap.appendChild(makeRow(tm,i)); });
  var cnt = document.querySelector('.concept-card.active .concept-card-count');
  if (cnt) cnt.textContent = terms.length + ' terms';
  var prev = terms.slice(0,2).map(function(t){return t.text;}).join(' OR ');
  var prevEl = document.querySelector('.concept-card.active .concept-card-preview');
  if (prevEl) prevEl.textContent = prev || 'No terms';
}

function makeRow(tm, i) {
  var row = document.createElement('div');
  row.className = 'term-row';

  // 1. Term text
  var textWrap = document.createElement('div');
  textWrap.style.minWidth = '0';
  var display = document.createElement('div');
  display.className = 'term-text-display';
  display.textContent = tm.text;
  display.title = 'Click to edit';
  display.addEventListener('click', function(){ startEditText(i,display,textWrap); });
  textWrap.appendChild(display);
  row.appendChild(textWrap);

  // 2. Flag chips (tiab / MeSH / exp / plain) — toggleable independently
  var chipsWrap = document.createElement('div');
  chipsWrap.className = 'type-chips';
  FLAG_DEFS.forEach(function(fd){
    var chip = document.createElement('button');
    chip.className = 'flag-chip' + (tm.flags.indexOf(fd.key)>=0 ? ' on' : '');
    chip.dataset.flag = fd.key;
    chip.textContent = fd.label;
    chip.title = 'Toggle ' + fd.label;
    chip.addEventListener('click', function(){
      var flags = terms[i].flags.slice();
      var idx = flags.indexOf(fd.key);
      if (idx>=0) { if(flags.length>1) flags.splice(idx,1); }
      else flags.push(fd.key);
      terms[i].flags = flags;
      pushTerms();
      renderAll();
    });
    chipsWrap.appendChild(chip);
  });
  row.appendChild(chipsWrap);

  // 3. Prox badge
  var proxWrap = document.createElement('div');
  proxWrap.className = 'prox-wrap';
  proxWrap.appendChild(makeProxEl(i, tm.prox));
  row.appendChild(proxWrap);

  // 4. Delete
  var del = document.createElement('button');
  del.className = 'btn-del';
  del.textContent = '×';
  del.addEventListener('click', function(){
    terms.splice(i,1); pushTerms(); renderAll();
  });
  row.appendChild(del);
  return row;
}

function makeProxEl(i, prox) {
  var lbl = proxLabel(prox);
  var badge = document.createElement('button');
  if (lbl) {
    badge.className = 'prox-badge';
    badge.textContent = lbl;
    badge.title = 'Click to edit, double-click to clear';
    badge.addEventListener('click', function(){ startEditProx(i,badge); });
    badge.addEventListener('dblclick', function(){ terms[i].prox=null; pushTerms(); renderAll(); });
  } else {
    badge.className = 'prox-badge empty';
    badge.textContent = '~/n';
    badge.title = 'Set proximity (NEAR/n)';
    badge.addEventListener('click', function(){ startEditProx(i,badge); });
  }
  return badge;
}

function startEditText(i, display, wrap) {
  var inp = document.createElement('input');
  inp.type='text'; inp.className='term-text-edit'; inp.value=terms[i].text;
  wrap.replaceChild(inp,display); inp.focus(); inp.select();
  function commit(){ var v=inp.value.trim(); if(v) terms[i].text=v; pushTerms(); renderAll(); }
  inp.addEventListener('blur',commit);
  inp.addEventListener('keydown',function(e){ if(e.key==='Enter'){e.preventDefault();inp.blur();} if(e.key==='Escape') renderAll(); });
}

function startEditProx(i, badge) {
  var inp = document.createElement('input');
  inp.type='number'; inp.min='1'; inp.max='20'; inp.className='prox-input-inline'; inp.placeholder='n';
  inp.value = (terms[i].prox!==null&&terms[i].prox!==undefined) ? terms[i].prox : '';
  badge.parentNode.replaceChild(inp,badge); inp.focus(); inp.select();
  function commit(){ var v=parseInt(inp.value,10); terms[i].prox=(!isNaN(v)&&v>0)?v:null; pushTerms(); renderAll(); }
  inp.addEventListener('blur',commit);
  inp.addEventListener('keydown',function(e){ if(e.key==='Enter'){e.preventDefault();inp.blur();} if(e.key==='Escape'){terms[i].prox=null;renderAll();} });
}

window.termEditor = {
  load: function(ci, termData) {
    conceptIdx = ci;
    terms = (termData||[]).map(function(t){
      return { text:t.text, flags: (t.flags&&t.flags.length)?t.flags:['tiab'], prox:t.prox||null };
    });
    renderAll();
  },
  addTerm: function(text) {
    if (!text.trim()) return;
    terms.push({text:text.trim(), flags:['tiab'], prox:null});
    pushTerms(); renderAll();
  }
};

})();
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(term_js)),
    tags$script(HTML("document.documentElement.setAttribute('data-theme','light');"))
  ),
  
  div(class="app-header",
      div(class="app-header-left",
          tags$span("🔬"),
          h1("Literature Search Builder"),
          div(class="app-header-sub","Build · Combine · Copy")
      ),
      div(class="io-toolbar",
          tags$button(id="theme_toggle",class="theme-btn",title="Toggle light/dark mode","☀️",
                      onclick="var h=document.documentElement,isLight=h.getAttribute('data-theme')==='light';h.setAttribute('data-theme',isLight?'dark':'light');this.textContent=isLight?'☀️':'🌙';"),
          downloadButton("save_rds",label=NULL,icon=icon("download"),class="io-btn save") |>
            (\(x){x$children[[2]]<-"Save Session";x})(),
          tags$label(`for`="load_file_input",class="io-btn load",tags$span("📂"),"Load Session"),
          tags$input(id="load_file_input",type="file",accept=".rds",
                     onchange="var f=this.files[0];if(!f)return;var r=new FileReader();r.onload=function(e){Shiny.setInputValue('load_file_data',{name:f.name,data:e.target.result.split(',')[1]},{priority:'event'});};r.readAsDataURL(f);this.value='';")
      )
  ),
  
  div(class="main-layout",
      
      # ── Left: concepts ──────────────────────────────────────────────────────────
      div(class="concepts-panel",
          div(class="panel-header",
              "Concepts",
              actionButton("add_concept","+ Add",class="btn-add-concept",
                           style="margin:0;padding:2px 9px;width:auto;font-size:.8rem;")
          ),
          div(class="concepts-list",uiOutput("concept_list")),
          tags$button("+ Add Concept",class="btn-add-concept",
                      onclick="Shiny.setInputValue('add_concept',Math.random(),{priority:'event'})")
      ),
      
      # ── Centre: editor ──────────────────────────────────────────────────────────
      div(class="editor-panel",
          div(class="editor-top",uiOutput("concept_editor")),
          div(class="output-panel",
              div(class="output-top-row",
                  div(class="db-tabs",
                      lapply(names(db_rules),function(db)
                        tags$button(db_rules[[db]]$name,class="db-tab",id=paste0("tab_",db),
                                    onclick=paste0("Shiny.setInputValue('selected_db','",db,"',{priority:'event'})"))
                      )
                  ),
                  div(class="included-summary",uiOutput("included_summary_ui"))
              ),
              div(class="query-box",id="query_display",uiOutput("query_output")),
              tags$button("Copy",id="copy_btn",class="copy-btn",
                          onclick="var t=document.getElementById('query_display').innerText;navigator.clipboard.writeText(t).then(function(){var b=document.getElementById('copy_btn');b.textContent='Copied!';b.classList.add('copied');setTimeout(function(){b.textContent='Copy';b.classList.remove('copied');},1600);});")
          )
      ),
      
      # ── Right: word bucket ──────────────────────────────────────────────────────
      div(class="bucket-panel",
          div(class="bucket-header",
              "Word Bucket",
              uiOutput("bucket_count_ui")
          ),
          div(class="bucket-search",
              tags$input(id="bucket_search",class="bucket-search-input",type="text",
                         placeholder="Filter…",
                         oninput="Shiny.setInputValue('bucket_filter',this.value)")
          ),
          div(class="bucket-list",uiOutput("bucket_list_ui")),
          tags$button("+ Add Selected to Concept",class="bucket-use-btn",id="bucket_use_btn",
                      onclick="Shiny.setInputValue('bucket_use',Math.random(),{priority:'event'})"),
          div(class="bucket-add-row",
              tags$input(id="bucket_new_input",class="bucket-new-input",type="text",
                         placeholder="Add to bucket…"),
              tags$button("Add",class="bucket-add-btn",
                          onclick="Shiny.setInputValue('bucket_add_btn',Math.random(),{priority:'event'})")
          )
      )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  concepts        <- reactiveVal(list(list(name="Concept 1",terms=list(),included=TRUE)))
  active_concept  <- reactiveVal(1L)
  selected_db     <- reactiveVal("pubmed")
  word_bucket     <- reactiveVal(character(0))   # all saved words/phrases
  bucket_selected <- reactiveVal(character(0))   # currently highlighted
  bucket_filter   <- reactiveVal("")
  
  # ── DB tabs ───────────────────────────────────────────────────────────────────
  observe({ runjs("document.getElementById('tab_pubmed').classList.add('active')") })
  observeEvent(input$selected_db,{
    selected_db(input$selected_db)
    lapply(names(db_rules),function(db){
      fn <- if(db==input$selected_db)"add" else "remove"
      runjs(sprintf("document.getElementById('tab_%s').classList.%s('active')",db,fn))
    })
  })
  
  # ── Concept management ────────────────────────────────────────────────────────
  observeEvent(input$add_concept,{
    cons <- concepts(); n <- length(cons)+1
    cons[[n]] <- list(name=paste("Concept",n),terms=list(),included=TRUE)
    concepts(cons); active_concept(n)
  },ignoreInit=TRUE)
  
  observeEvent(input$select_concept,{ active_concept(as.integer(input$select_concept)) })
  
  observeEvent(input$delete_concept,{
    idx <- as.integer(input$delete_concept); cons <- concepts()
    cons[[idx]] <- NULL; concepts(cons)
    ac <- active_concept()
    if (!length(cons)) active_concept(NULL)
    else active_concept(min(max(1L,if(ac>=idx) ac-1L else ac),length(cons)))
  })
  
  observeEvent(input$toggle_include,{
    idx <- as.integer(input$toggle_include); cons <- concepts()
    if (!is.null(cons[[idx]])){cons[[idx]]$included <- !isTRUE(cons[[idx]]$included);concepts(cons)}
  })
  
  observeEvent(input$concept_name_commit,{
    ac <- active_concept(); if(is.null(ac)) return()
    cons <- concepts()
    if (!is.null(cons[[ac]])){cons[[ac]]$name <- input$concept_name_commit;concepts(cons)}
  },ignoreInit=TRUE)
  
  # ── Terms update from JS ──────────────────────────────────────────────────────
  observeEvent(input$terms_update,{
    ci    <- as.integer(input$terms_update$ci)
    tlist <- input$terms_update$terms
    cons  <- concepts(); if(is.null(cons[[ci]])) return()
    cons[[ci]]$terms <- lapply(tlist,function(t) list(
      text  = as.character(t$text %||% ""),
      flags = if(length(t$flags)) as.character(unlist(t$flags)) else "tiab",
      prox  = {v<-suppressWarnings(as.integer(t$prox));if(is.na(v)||v<=0||!isTruthy(v)) NA_integer_ else v}
    ))
    concepts(cons)
    # Also auto-add terms to bucket
    new_words <- sapply(tlist,function(t) trimws(as.character(t$text %||% "")))
    new_words <- new_words[nzchar(new_words)]
    wb <- word_bucket()
    added <- setdiff(new_words, wb)
    if (length(added)) word_bucket(c(wb,added))
  },ignoreInit=TRUE)
  
  # ── Add term ─────────────────────────────────────────────────────────────────
  observeEvent(input$add_term_btn,{
    txt <- trimws(input$new_term_text %||% "")
    if (!nzchar(txt)) return()
    runjs(sprintf("if(window.termEditor) window.termEditor.addTerm(%s);",jsonlite::toJSON(txt,auto_unbox=TRUE)))
    updateTextInput(session,"new_term_text",value="")
    runjs("setTimeout(function(){var e=document.getElementById('new_term_text');if(e)e.focus();},80);")
  })
  
  observe({
    runjs("$(document).off('keydown.addterm').on('keydown.addterm','#new_term_text',function(e){if(e.key==='Enter'){e.preventDefault();Shiny.setInputValue('add_term_btn',Math.random(),{priority:'event'});}});")
  })
  
  # ── Bucket: add new word manually ────────────────────────────────────────────
  add_to_bucket <- function(txt) {
    txt <- trimws(txt)
    if (!nzchar(txt)) return()
    wb <- word_bucket()
    if (!txt %in% wb) word_bucket(c(wb, txt))
    updateTextInput(session,"bucket_new_input",value="")
    runjs("setTimeout(function(){var e=document.getElementById('bucket_new_input');if(e)e.focus();},80);")
  }
  
  observeEvent(input$bucket_add_btn,{ add_to_bucket(input$bucket_new_input %||% "") })
  
  observe({
    runjs("$(document).off('keydown.bucket').on('keydown.bucket','#bucket_new_input',function(e){if(e.key==='Enter'){e.preventDefault();Shiny.setInputValue('bucket_add_btn',Math.random(),{priority:'event'});}});")
  })
  
  # ── Bucket: filter ────────────────────────────────────────────────────────────
  observeEvent(input$bucket_filter,{ bucket_filter(input$bucket_filter %||% "") })
  
  # ── Bucket: toggle selection ──────────────────────────────────────────────────
  observeEvent(input$bucket_toggle_sel,{
    word <- input$bucket_toggle_sel
    sel  <- bucket_selected()
    if (word %in% sel) bucket_selected(setdiff(sel,word))
    else bucket_selected(c(sel,word))
  })
  
  # ── Bucket: delete item ───────────────────────────────────────────────────────
  observeEvent(input$bucket_delete,{
    word <- input$bucket_delete
    word_bucket(setdiff(word_bucket(),word))
    bucket_selected(setdiff(bucket_selected(),word))
  })
  
  # ── Bucket: use selected → add to active concept ──────────────────────────────
  observeEvent(input$bucket_use,{
    sel <- bucket_selected(); if(!length(sel)) return()
    ac  <- active_concept();  if(is.null(ac)) return()
    for (w in sel) {
      runjs(sprintf("if(window.termEditor) window.termEditor.addTerm(%s);",jsonlite::toJSON(w,auto_unbox=TRUE)))
    }
    bucket_selected(character(0))
    showNotification(paste0("Added ",length(sel)," term",if(length(sel)>1)"s" else ""," to concept"),type="message",duration=2)
  })
  
  # ── Save/Load ─────────────────────────────────────────────────────────────────
  output$save_rds <- downloadHandler(
    filename=function() paste0("search_session_",format(Sys.time(),"%Y%m%d_%H%M%S"),".rds"),
    content =function(file) saveRDS(list(
      version="2.0", saved_at=Sys.time(),
      concepts=concepts(), word_bucket=word_bucket()
    ),file)
  )
  
  observeEvent(input$load_file_data,{
    tryCatch({
      raw <- base64enc::base64decode(input$load_file_data$data)
      tmp <- tempfile(fileext=".rds"); writeBin(raw,tmp)
      sess <- readRDS(tmp); unlink(tmp)
      if (!is.list(sess)||is.null(sess$concepts)){showNotification("Invalid file.",type="error");return()}
      cons <- lapply(sess$concepts,function(c){
        if(is.null(c$included)) c$included <- TRUE
        # migrate old type field → flags
        c$terms <- lapply(c$terms,function(t){
          if(is.null(t$flags)) t$flags <- if(!is.null(t$type)) c(t$type) else c("tiab")
          t
        })
        c
      })
      concepts(cons); active_concept(if(length(cons)) 1L else NULL)
      if (!is.null(sess$word_bucket)) word_bucket(as.character(sess$word_bucket))
      bucket_selected(character(0))
      showNotification(paste0("Loaded: ",input$load_file_data$name," (",length(cons)," concepts)"),type="message",duration=3)
    },error=function(e) showNotification(paste("Load failed:",e$message),type="error"))
  })
  
  # ── Render concept list ───────────────────────────────────────────────────────
  output$concept_list <- renderUI({
    cons <- concepts(); ac <- active_concept()
    if (!length(cons)) return(div(class="empty-state",div(class="icon","📋"),"No concepts yet"))
    tagList(lapply(seq_along(cons),function(i){
      con <- cons[[i]]; inc <- isTRUE(con$included); n_t <- length(con$terms)
      prev <- if(n_t) paste(sapply(con$terms[seq_len(min(2,n_t))],`[[`,"text"),collapse=" OR ") else "No terms"
      cls  <- paste0("concept-card",if(!is.null(ac)&&ac==i)" active" else "",if(!inc)" excluded" else "")
      div(class=cls,
          div(class="concept-card-header",
              tags$button(class=paste0("include-toggle",if(inc)" on" else ""),
                          title=if(inc)"Included" else "Excluded",
                          onclick=paste0("Shiny.setInputValue('toggle_include','",i,"',{priority:'event'});event.stopPropagation();")),
              span(class="concept-card-label",id=paste0("card_label_",i),
                   paste0("C",i,": ",con$name),
                   onclick=paste0("Shiny.setInputValue('select_concept','",i,"',{priority:'event'})")),
              span(class="concept-card-count",paste(n_t,"terms")),
              tags$button("×",class="btn-del-concept",
                          onclick=paste0("Shiny.setInputValue('delete_concept','",i,"',{priority:'event'});event.stopPropagation();"))
          ),
          div(class="concept-card-preview",prev,
              onclick=paste0("Shiny.setInputValue('select_concept','",i,"',{priority:'event'})"))
      )
    }))
  })
  
  # ── Render editor ─────────────────────────────────────────────────────────────
  output$concept_editor <- renderUI({
    ac   <- active_concept()
    cons <- isolate(concepts())
    if (is.null(ac)||!length(cons)||ac>length(cons))
      return(div(class="empty-state",div(class="icon","📋"),"Select or add a concept to start building"))
    
    con      <- cons[[ac]]
    included <- isTRUE(con$included)
    term_json <- jsonlite::toJSON(
      lapply(con$terms,function(t) list(
        text  = t$text,
        flags = if(length(t$flags)) as.list(t$flags) else list("tiab"),
        prox  = if(is.null(t$prox)||is.na(t$prox)) NULL else t$prox
      )),
      auto_unbox=TRUE, null="null"
    )
    
    tagList(
      div(class="concept-title-row",
          tags$input(id="concept_name_edit",class="concept-title-input",type="text",
                     value=con$name,placeholder="Concept name…",
                     oninput=paste0("document.getElementById('card_label_",ac,"').textContent='C",ac,": '+this.value;"),
                     onblur="Shiny.setInputValue('concept_name_commit',this.value,{priority:'event'})",
                     onkeydown="if(event.key==='Enter'){Shiny.setInputValue('concept_name_commit',this.value,{priority:'event'});this.blur();}"
          ),
          span(class=paste0("include-badge ",if(included)"on" else "off"),
               if(included)"✓ In Query" else "✗ Excluded")
      ),
      
      div(style="font-size:.76rem;color:var(--tx-mut);margin-bottom:7px;flex-shrink:0;",
          "Click a term to edit text. Toggle ",tags$strong("tiab / MeSH / exp / plain")," chips independently. ",
          "Click ",tags$strong("~/n")," to set proximity."
      ),
      
      div(class="terms-list-wrap", div(id="terms-list")),
      
      div(class="add-term-row",
          tags$input(id="new_term_text",class="term-new-input",type="text",
                     placeholder="Type a term and press Enter or click Add…"),
          tags$button("+ Add Term",class="btn-add-term",
                      onclick="Shiny.setInputValue('add_term_btn',Math.random(),{priority:'event'})")
      ),
      
      tags$script(HTML(sprintf(
        "setTimeout(function(){if(window.termEditor) window.termEditor.load(%d,%s);},50);",
        ac, term_json
      )))
    )
  })
  
  # ── Word bucket UI ────────────────────────────────────────────────────────────
  output$bucket_count_ui <- renderUI({
    n <- length(word_bucket())
    span(class="bucket-count", paste(n,"words"))
  })
  
  output$bucket_list_ui <- renderUI({
    wb  <- word_bucket()
    sel <- bucket_selected()
    flt <- tolower(trimws(bucket_filter()))
    if (!length(wb))
      return(div(class="empty-state",div(class="icon","🗂️"),"Add words here to reuse across concepts"))
    
    if (nzchar(flt)) wb <- wb[grepl(flt,tolower(wb),fixed=TRUE)]
    if (!length(wb))
      return(div(style="padding:10px;font-size:.82rem;color:var(--tx-faint);","No matches"))
    
    tagList(lapply(wb, function(w) {
      is_sel <- w %in% sel
      div(class=paste0("bucket-item",if(is_sel)" selected" else ""),
          span(class="bucket-item-text", title=w, w),
          tags$button("×",class="bucket-item-del",title="Remove from bucket",
                      onclick=paste0("Shiny.setInputValue('bucket_delete',",jsonlite::toJSON(w,auto_unbox=TRUE),",{priority:'event'});event.stopPropagation();"))
      ) |> tagAppendAttributes(
        onclick=paste0("Shiny.setInputValue('bucket_toggle_sel',",jsonlite::toJSON(w,auto_unbox=TRUE),",{priority:'event'});")
      )
    }))
  })
  
  # ── Included summary ─────────────────────────────────────────────────────────
  output$included_summary_ui <- renderUI({
    cons  <- concepts()
    n_inc <- sum(vapply(cons,function(c) isTRUE(c$included),logical(1)))
    HTML(sprintf('<span>%d</span>/%d concepts in query',n_inc,length(cons)))
  })
  
  # ── Query output ──────────────────────────────────────────────────────────────
  output$query_output <- renderUI({
    cons <- concepts(); db <- selected_db()
    if (!length(cons)) return("— add concepts and terms to generate a query —")
    q <- tryCatch(build_query(cons,db),error=function(e) paste("Error:",e$message))
    HTML(htmltools::htmlEscape(q))
  })
}

`%||%` <- function(a,b) if(!is.null(a)) a else b

shinyApp(ui, server)