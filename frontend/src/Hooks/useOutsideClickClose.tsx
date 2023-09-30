import { onCleanup } from "solid-js";

/**
 * Hook to handle outside clicks to close a dropdown or modal.
 * @param {() => boolean} getIsVisible Getter function for the signal indicating if the dropdown/modal is visible.
 * @param {(value: boolean) => void} setIsVisible Setter function for the signal.
 * @param {string} containerClass Class of the dropdown/modal container.
 * @param {string[]} [preventCloseClasses] Optional list of classes that prevent the dropdown from closing when clicked.
 */
function useOutsideClickClose(
  getIsVisible: () => boolean,
  setIsVisible: (value: boolean) => void,
  containerClass: string,
  preventCloseClasses: string[] = []
) {
  if (getIsVisible()) {
    const handleClickOutside = (event: Event) => {
      // Type assertion for event.target
      const targetElement = event.target as HTMLElement;

      // If the clicked element has a class that prevents closing, return early
      for (const preventClass of preventCloseClasses) {
        if (targetElement.classList.contains(preventClass)) {
          return;
        }
      }

      // If click is outside the container, close the dropdown/modal
      const container = document.querySelector("." + containerClass);
      if (container && !container.contains(targetElement)) {
        setIsVisible(false);
      }
    };

    document.addEventListener("click", handleClickOutside);

    // Cleanup: remove the event listener
    onCleanup(() => {
      document.removeEventListener("click", handleClickOutside);
    });
  }
}

export default useOutsideClickClose;
