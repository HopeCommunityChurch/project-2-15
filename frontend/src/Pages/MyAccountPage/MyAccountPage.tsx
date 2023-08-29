import { createEffect, createSignal, onMount, For, createResource, Show } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { MyAccountTopNav } from "../../Components/MyAccountTopNav/MyAccountTopNav";
import BlueCheckIcon from "../../Assets/blue-check-icon.svg";
import GrayCircleIcon from "../../Assets/gray-circle-icon.svg";
import Arrow2Icon from "../../Assets/arrow2.svg";

import * as classes from "./styles.module.scss";

export function MyAccountPage() {
  const dummyUserInfo = {
    firstName: "Jon",
    lastName: "Boi",
    email: "jonboimaximus2000@yurrr.com",
    phoneNumber: "",
    preferredLang: "es",
  };
  const [isImageUploaded, setIsImageUploaded] = createSignal(false);
  const [firstName, setFirstName] = createSignal(dummyUserInfo.firstName);
  const [lastName, setLastName] = createSignal(dummyUserInfo.lastName);
  const [email, setEmail] = createSignal(dummyUserInfo.email);
  const [phoneNumber, setPhoneNumber] = createSignal(dummyUserInfo.phoneNumber);
  const [preferredLang, setPreferredLang] = createSignal(dummyUserInfo.preferredLang);
  const [hasPersoanlInfoChanged, setHasPersoanlInfoChanged] = createSignal(false);
  const [curPass, setCurPass] = createSignal("");
  const [newPass, setNewPass] = createSignal("");
  const [confirmNewPass, setConfirmNewPass] = createSignal("");

  const handleImageUpload = (e: Event) => {
    const fileInput = e.target as HTMLInputElement;
    if (fileInput.files && fileInput.files.length > 0) {
      setIsImageUploaded(true);
    } else {
      setIsImageUploaded(false);
    }
  };

  // Derived state to check if any field value has changed
  createEffect(() => {
    setHasPersoanlInfoChanged(
      firstName() !== dummyUserInfo.firstName ||
        lastName() !== dummyUserInfo.lastName ||
        email() !== dummyUserInfo.email ||
        phoneNumber() !== dummyUserInfo.phoneNumber ||
        preferredLang() !== dummyUserInfo.preferredLang
    );
  });

  const handleUpdateProfilePicture = () => {
    // Logic to update the profile picture
  };

  const handleUpdatePersonalInformation = (e: Event) => {
    e.preventDefault();
    // Logic to update personal information
  };

  const handleChangePassword = (e: Event) => {
    e.preventDefault();
    // Logic to change the password
  };

  const handleDeleteAccount = () => {
    // Logic to delete the account
  };
  return (
    <>
      <MyAccountTopNav />
      <div class={classes.pageBody}>
        <h1 class={classes.title}>My Account</h1>

        {/* Profile Picture Section */}
        <section class={classes.profilePictureSection}>
          <h2 class={classes.sectionTitle}>Profile Picture</h2>
          <div class={classes.profilePicAndUpdateOptions}>
            <img
              class={classes.profilePicture}
              src="default-profile-picture.jpg"
              alt="Profile Picture"
            />
            <div class={classes.updateProfilePicture}>
              <p>Want to update your picture?</p>
              <input
                class={classes.fileInput}
                type="file"
                id="profile-picture-upload"
                accept="image/*"
                onChange={handleImageUpload}
              />
              <button
                class={classes.button}
                type="button"
                id="update-profile-picture"
                onClick={handleUpdateProfilePicture}
                disabled={!isImageUploaded()}
              >
                Update Picture
              </button>
            </div>
          </div>
        </section>

        {/* Personal Information Section */}
        <section class={classes.personalInformationSection}>
          <h2 class={classes.sectionTitle}>Personal Information</h2>
          <form id="personal-information-form" onSubmit={handleUpdatePersonalInformation}>
            <label class={classes.label} for="first-name">
              First Name:
            </label>
            <input
              class={classes.input}
              type="text"
              id="first-name"
              name="first-name"
              value={firstName()}
              onInput={(e) => setFirstName(e.currentTarget.value)}
            />

            <label class={classes.label} for="last-name">
              Last Name:
            </label>
            <input
              class={classes.input}
              type="text"
              id="last-name"
              name="last-name"
              value={lastName()}
              onInput={(e) => setLastName(e.currentTarget.value)}
            />

            <label class={classes.label} for="email">
              Email:
            </label>
            <input
              class={classes.input}
              type="email"
              id="email"
              name="email"
              value={email()}
              onInput={(e) => setEmail(e.currentTarget.value)}
            />

            <label class={classes.label} for="phone">
              Phone Number:
            </label>
            <input
              class={classes.input}
              type="tel"
              id="phone"
              name="phone"
              value={phoneNumber()}
              onInput={(e) => setPhoneNumber(e.currentTarget.value)}
            />

            <label class={classes.label} for="language">
              Preferred Language:
            </label>
            <select
              class={classes.select}
              id="language"
              name="language"
              value={preferredLang()}
              onInput={(e) => setPreferredLang(e.currentTarget.value)}
            >
              <option value="en">English</option>
              <option value="es">Spanish</option>
              <option value="fr">French</option>
              {/* Add other languages here */}
            </select>

            <button
              class={classes.button}
              type="submit"
              id="update-personal-information"
              disabled={!hasPersoanlInfoChanged()}
            >
              Update Information
            </button>
          </form>
        </section>

        {/* Account Settings Section */}
        <section class={classes.accountSettingsSection}>
          <h2 class={classes.sectionTitle}>Account Settings</h2>

          {/* Change Password */}
          <form id="change-password-form" onSubmit={handleChangePassword}>
            <label class={classes.label} for="current-password">
              Current Password:
            </label>
            <input
              class={classes.input}
              type="password"
              id="current-password"
              name="current-password"
              onInput={(e) => setCurPass(e.currentTarget.value)}
            />

            <label class={classes.label} for="new-password">
              New Password:
            </label>
            <input
              class={classes.input}
              type="password"
              id="new-password"
              name="new-password"
              onInput={(e) => setNewPass(e.currentTarget.value)}
            />

            <label class={classes.label} for="confirm-password">
              Confirm New Password:
            </label>
            <input
              class={classes.input}
              type="password"
              id="confirm-password"
              name="confirm-password"
              onInput={(e) => setConfirmNewPass(e.currentTarget.value)}
            />

            <button
              class={classes.button}
              type="submit"
              id="change-password"
              disabled={
                curPass() === "" ||
                newPass() === "" ||
                confirmNewPass() === "" ||
                newPass() != confirmNewPass()
              }
            >
              Change Password
            </button>
          </form>

          {/* Delete Account */}
          <button
            class={classes.deleteButton}
            type="button"
            id="delete-account"
            onClick={handleDeleteAccount}
          >
            Delete Account
          </button>
        </section>
      </div>
    </>
  );
}
