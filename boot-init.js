export default function hbInitializer() {
  if (window.__HB_TRUNK_INIT) {
    return window.__HB_TRUNK_INIT();
  }
  return {};
}
