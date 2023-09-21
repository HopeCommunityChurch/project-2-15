import { createEffect, createSignal, Show } from "solid-js";
import { useNavigate } from "@solidjs/router";
import { A } from "@solidjs/router";
import CloseXIcon from "Assets/x.svg";
import MagnifyingGlassWhiteIcon from "Assets/magnifying_glass_white.png";
import ArrowIcon from "Assets/arrow.svg";
import { DynamicTextArea } from "./DynamicTextArea/DynamicTextArea";
import { VerseReferenceInput } from "./VerseReferenceInput/VerseReferenceInput";
import { isValidVerseReference } from "./isValidVerseReference";

import * as classes from "./styles.module.scss";

export function SectionEditor({ showSectionEditor, setShowSectionEditor }) {
  const [sectionPassages, setSectionPassages] = createSignal({
    Sections: [
      {
        SectionTitle: "Romans 1:1-7",
        ListOfVerses: [
          {
            VerseReference: "Romans 1:1-7",
            Scripture:
              "Paul, a servant of Christ Jesus, called to be an apostle, set apart for the gospel of God, 2 which he promised beforehand through his prophets in the holy Scriptures, 3 concerning his Son, who was descended from David according to the flesh 4 and was declared to be the Son of God in power according to the Spirit of holiness by his resurrection from the dead, Jesus Christ our Lord, 5 through whom we have received grace and apostleship to bring about the obedience of faith for the sake of his name among all the nations, 6 including you who are called to belong to Jesus Christ, 7 To all those in Rome who are loved by God and called to be saints: Grace to you and peace from God our Father and the Lord Jesus Christ.",
          },
        ],
      },
      {
        SectionTitle: "1 Cor (various)",
        ListOfVerses: [
          {
            VerseReference: "1 Cor 1:1-3",
            Scripture:
              "Paul, called by the will of God to be an apostle of Christ Jesus, and our brother Sosthenes,2 To the church of God that is in Corinth, to those sanctified in Christ Jesus, called to be saints together with all those who in every place call upon the name of our Lord Jesus Christ, both their Lord and ours: 3 Grace to you and peace from God our Father and the Lord Jesus Christ.",
          },
          {
            VerseReference: "1 Cor 1:26-31",
            Scripture:
              "26 For consider your calling, brothers: not many of you were wise according to worldly standards, not many were powerful, not many were of noble birth. 27 But God chose what is foolish in the world to shame the wise; God chose what is weak in the world to shame the strong; 28 God chose what is low and despised in the world, even things that are not, to bring to nothing things that are, 29 so that no human being might boast in the presence of God. 30 And because of him you are in Christ Jesus, who became to us wisdom from God, righteousness and sanctification and redemption, 31 so that, as it is written, “Let the one who boasts, boast in the Lord.” 26 For consider your calling, brothers: not many of you were wise according to worldly standards, not many were powerful, not many were of noble birth. 27 But God chose what is foolish in the world to shame the wise; God chose what is weak in the world to shame the strong; 28 God chose what is low and despised in the world, even things that are not, to bring to nothing things that are, 29 so that no human being might boast in the presence of God. 30 And because of him you are in Christ Jesus, who became to us wisdom from God, righteousness and sanctification and redemption, 31 so that, as it is written, “Let the one who boasts, boast in the Lord.”",
          },
        ],
      },
    ],
  });

  const handleScriptureChange = (sectionIndex, verseIndex, newValue) => {
    setSectionPassages((prev) => {
      const newSections = [...prev.Sections];
      newSections[sectionIndex].ListOfVerses[verseIndex].Scripture = newValue;
      return { Sections: newSections };
    });
  };

  const moveSectionUp = (index) => {
    setSectionPassages((prev) => {
      if (index === 0) return prev; // If it's the first element, no move
      const newSections = [...prev.Sections];
      [newSections[index - 1], newSections[index]] = [newSections[index], newSections[index - 1]];
      return { Sections: newSections };
    });
  };

  const moveSectionDown = (index) => {
    setSectionPassages((prev) => {
      if (index === prev.Sections.length - 1) return prev; // If it's the last element, no move
      const newSections = [...prev.Sections];
      [newSections[index + 1], newSections[index]] = [newSections[index], newSections[index + 1]];
      return { Sections: newSections };
    });
  };

  const moveVerseUp = (sectionIndex, verseIndex) => {
    setSectionPassages((prev) => {
      if (verseIndex === 0) return prev; // If it's the first element, no move
      const newSections = [...prev.Sections];
      const verses = newSections[sectionIndex].ListOfVerses;
      [verses[verseIndex - 1], verses[verseIndex]] = [verses[verseIndex], verses[verseIndex - 1]];
      return { Sections: newSections };
    });
  };

  const moveVerseDown = (sectionIndex, verseIndex) => {
    setSectionPassages((prev) => {
      if (verseIndex === prev.Sections[sectionIndex].ListOfVerses.length - 1) return prev; // If it's the last element, no move
      const newSections = [...prev.Sections];
      const verses = newSections[sectionIndex].ListOfVerses;
      [verses[verseIndex + 1], verses[verseIndex]] = [verses[verseIndex], verses[verseIndex + 1]];
      return { Sections: newSections };
    });
  };

  const handleVerseReferenceChange = (sectionIndex, verseIndex, newValue) => {
    setSectionPassages((prev) => {
      const newSections = [...prev.Sections];
      newSections[sectionIndex].ListOfVerses[verseIndex].VerseReference = newValue;
      return { Sections: newSections };
    });
  };

  const validateScriptureReference = (sectionIndex, verseIndex) => {
    console.log(
      isValidVerseReference(
        sectionPassages().Sections[sectionIndex].ListOfVerses[verseIndex].VerseReference
      )
    );
    // Add your validation logic here
  };

  return (
    <Show when={showSectionEditor()}>
      <div class={classes.modalBackground} onClick={() => setShowSectionEditor(false)}>
        <div class={classes.ModalBody} onClick={(e) => e.stopPropagation()}>
          <div class={classes.ModalHeader}>
            <h3>Edit Sections</h3>
            <div class={classes.addSectionButton}>+ Add Section</div>
            <div class={classes.saveButton}>Save</div>
            <img
              src={CloseXIcon}
              alt="Close Modal"
              class={classes.closeModalIcon}
              onClick={() => setShowSectionEditor(false)}
            />
          </div>
          {sectionPassages().Sections.map((section, sectionIndex) => (
            <div>
              <div class={classes.sectionHeader}>
                <img src={ArrowIcon} class={classes.sectionExpandIcon} />
                <div class={classes.reorderSectionContainer}>
                  <img
                    src={ArrowIcon}
                    class={classes.upIcon}
                    onClick={() => moveSectionUp(sectionIndex)}
                  />
                  <img
                    src={ArrowIcon}
                    class={classes.downIcon}
                    onClick={() => moveSectionDown(sectionIndex)}
                  />
                </div>
                <input type="text" value={section.SectionTitle} />
                <img src={CloseXIcon} class={classes.closeSectionIcon} />
              </div>
              <div class={classes.sectionScriptureBody}>
                {section.ListOfVerses.map((verse, verseIndex) => (
                  <div class={classes.scriptureContainer}>
                    <div class={classes.reorderScriptureContainer}>
                      <img
                        src={ArrowIcon}
                        class={classes.upIcon}
                        onClick={() => moveVerseUp(sectionIndex, verseIndex)}
                      />
                      <img
                        src={ArrowIcon}
                        class={classes.downIcon}
                        onClick={() => moveVerseDown(sectionIndex, verseIndex)}
                      />
                    </div>

                    <div class={classes.scriptureSelectAndPassage}>
                      <div class={classes.verseRangeSelectorAndCloseIcon}>
                        <VerseReferenceInput
                          initialValue={verse.VerseReference}
                          sectionIndex={sectionIndex}
                          verseIndex={verseIndex}
                          handleVerseReferenceChange={handleVerseReferenceChange}
                        />
                        <div
                          class={classes.updateScriptureButton}
                          onClick={() => validateScriptureReference(sectionIndex, verseIndex)}
                        >
                          <img src={MagnifyingGlassWhiteIcon} />
                        </div>
                        <img src={CloseXIcon} class={classes.closeScriptureIcon} />
                      </div>
                      <DynamicTextArea
                        scripture={verse.Scripture}
                        onScriptureChange={(newValue) =>
                          handleScriptureChange(sectionIndex, verseIndex, newValue)
                        }
                      />
                    </div>
                  </div>
                ))}
              </div>
              <div
                class={classes.addScriptureButton}
                onClick={() => {
                  /* Add functionality to add a new scripture here */
                }}
              >
                + Add Scripture
              </div>
            </div>
          ))}
        </div>
      </div>
    </Show>
  );
}
