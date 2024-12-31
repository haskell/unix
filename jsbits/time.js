//#OPTIONS: CPP

function h$js_futimes(fd,atime,mtime) {
#ifdef GHCJS_BROWSER
    throw "h$js_futimes unsupported";
#else
  if (!h$isNode()) {
    throw "h$js_futimes unsupported";
  }
  try {
    h$fs.futimesSync(fd, atime, mtime);
  } catch(e) {
    h$setErrno(e);
    return -1;
  }
  return 0;
#endif
}

function h$js_utimes(path,path_offset,atime,mtime) {
#ifdef GHCJS_BROWSER
    throw "h$js_utimes unsupported";
#else
  if (!h$isNode()) {
    throw "h$js_utimes unsupported";
  }
  try {
    const d = h$decodeUtf8z(path, path_offset);
    h$fs.utimesSync(d, atime, mtime);
  } catch(e) {
    h$setErrno(e);
    return -1;
  }
  return 0;
#endif
}

function h$js_lutimes(path,path_offset,atime,mtime) {
#ifdef GHCJS_BROWSER
    throw "h$js_lutimes unsupported";
#else
  if (!h$isNode()) {
    throw "h$js_lutimes unsupported";
  }
  try {
    const d = h$decodeUtf8z(path, path_offset);
    h$fs.lutimesSync(d, atime, mtime);
  } catch(e) {
    h$setErrno(e);
    return -1;
  }
  return 0;
#endif
}

