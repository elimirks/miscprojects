<script lang="ts">
    import { vowels, consonants } from "$lib";

    const textHeight = 20;
    const padding = 5.0;
    const charWidth = 45.0;
    const charHeight = 80.0;
    // Bitmasks of 16 bits each
    export let chars: number[] = [];
    let charCount = chars.length;

    enum Certainty {
        Unknown,
        Suggested,
        Likely,
        Definite,
    }

    type Sound = {
        certainty: Certainty;
        text: string;
    }

    let consonantMapping: Map<number, Sound> = new Map();
    consonantMapping.set(consonants[14], {
        certainty: Certainty.Definite,
        text: "ð",
    });
    consonantMapping.set(consonants[13], {
        certainty: Certainty.Definite,
        text: "θ",
    });
    // Found in "shield". All other phonemes are known, so it must be "l"
    consonantMapping.set(consonants[23], {
        certainty: Certainty.Definite,
        text: "l",
    });
    // Found in "shield", "cards"
    consonantMapping.set(consonants[6], {
        certainty: Certainty.Definite,
        text: "d",
    });
    // Found in "shield", "shore"
    consonantMapping.set(consonants[17], {
        certainty: Certainty.Definite,
        text: "sh",
    });
    // Found in "key" and "cards" (?)
    consonantMapping.set(consonants[7], {
        certainty: Certainty.Definite,
        text: "k",
    });
    // Found in "cards" (?) and "wells" (?)
    // Hard s or z
    consonantMapping.set(consonants[16], {
        certainty: Certainty.Suggested,
        text: "z",
    });
    // Found in "west", "wells" (?), possible "when" on page 20, cyan peril ring
    consonantMapping.set(consonants[22], {
        certainty: Certainty.Suggested,
        text: "w",
    });
    // Found in "east", "south"
    // Soft s
    consonantMapping.set(consonants[15], {
        certainty: Certainty.Definite,
        text: "s",
    });
    // Found in "north", "button" (?)
    consonantMapping.set(consonants[1], {
        certainty: Certainty.Definite,
        text: "n",
    });
    // Found in "light", "lost"
    consonantMapping.set(consonants[5], {
        certainty: Certainty.Definite,
        text: "t",
    });
    // Found in "controls"
    consonantMapping.set(consonants[20], {
        certainty: Certainty.Suggested,
        text: "r",
    });
    // Found in "move", "item"
    consonantMapping.set(consonants[0], {
        certainty: Certainty.Definite,
        text: "m",
    });
    // Found in "move" (?)
    consonantMapping.set(consonants[12], {
        certainty: Certainty.Suggested,
        text: "v",
    });
    // Found in "potion"
    consonantMapping.set(consonants[3], {
        certainty: Certainty.Suggested,
        text: "p",
    });
    // Found in "use", "you"
    consonantMapping.set(consonants[21], {
        certainty: Certainty.Suggested,
        text: "y",
    });
    // Found in "focus", "far", "find"
    consonantMapping.set(consonants[11], {
        certainty: Certainty.Definite,
        text: "f",
    });
    // Found in "button"
    consonantMapping.set(consonants[4], {
        certainty: Certainty.Suggested,
        text: "b",
    });
    // Soft ng, Found in "beings" (?)
    consonantMapping.set(consonants[2], {
        certainty: Certainty.Suggested,
        text: "ng",
    });
    // Found in "golden" (?) page 54
    consonantMapping.set(consonants[8], {
        certainty: Certainty.Suggested,
        text: "g",
    });
    // Found in "strange" (?)
    consonantMapping.set(consonants[9], {
        certainty: Certainty.Suggested,
        text: "j",
    });
    let vowelMapping: Map<number, Sound> = new Map();
    // Found in "the" and "a", potion
    vowelMapping.set(vowels[5], {
        certainty: Certainty.Definite,
        text: "ə",
    });
    // Same sound in "key", "east", "shield". Pretty likely.
    vowelMapping.set(vowels[6], {
        certainty: Certainty.Definite,
        text: "ee",
    });
    // Soft e, found in "west", "wells" (?), possibly "when" on page 20, cyan peril ring
    vowelMapping.set(vowels[3], {
        certainty: Certainty.Likely,
        text: "e",
    });
    // As in "car", or "are"
    vowelMapping.set(vowels[10], {
        certainty: Certainty.Definite,
        text: "ar",
    });
    // As in "shore" or "north"
    vowelMapping.set(vowels[11], {
        certainty: Certainty.Definite,
        text: "or",
    });
    // As in "light", "eye"
    vowelMapping.set(vowels[13], {
        certainty: Certainty.Suggested,
        text: "ai",
    });
    vowelMapping.set(vowels[1], {
        certainty: Certainty.Suggested,
        text: "aw",
    });
    // As in "contrOls", "awl"
    vowelMapping.set(vowels[16], {
        certainty: Certainty.Suggested,
        text: "o",
    });
    // As in "to the far shore" (teleporter on page 11), "you", "move"
    vowelMapping.set(vowels[7], {
        certainty: Certainty.Definite,
        text: "oo",
    });
    // As in "visit", "button", "will"
    vowelMapping.set(vowels[2], {
        certainty: Certainty.Suggested,
        text: "ih",
    });
    // As in "vigOR", "travERse"
    vowelMapping.set(vowels[8], {
        certainty: Certainty.Suggested,
        text: "er",
    });
    // Found in "strange" (?), "gate", "greatest"
    vowelMapping.set(vowels[12], {
        certainty: Certainty.Suggested,
        text: "ay",
    });
    // Found in "as", "path"
    vowelMapping.set(vowels[0], {
        certainty: Certainty.Suggested,
        text: "ah",
    });

    function extractVowel(c: number): number {
        return c & 0b1010010011001000;
    }

    function extractConsonant(c: number): number {
        return c & 0b0101100100110100;
    }

    function isFlipped(c: number): boolean {
        return (c & 1) == 1;
    }

    function charText(c: number): string {
        let conText = "?";
        let vowText = "?";

        const con = extractConsonant(c);
        const vow = extractVowel(c);

        if (con == 0 && vow == 0) {
            return "";
        }
        if (con == 0) {
            conText = "";
        } else {
            const conSound = consonantMapping.get(con);
            if (conSound != undefined) {
                conText = conSound.text;
            }
        }
        if (vow == 0) {
            vowText = "";
        } else {
            const vowSound = vowelMapping.get(vow);
            if (vowSound != undefined) {
                vowText = vowSound.text;
            }
        }

        const dash = (vow == 0 || con == 0) ? "" : "-";
        if (isFlipped(c)) {
            return `${vowText}${dash}${conText}`;
        } else {
            return `${conText}${dash}${vowText}`;
        }
    }

    function charColor(c: number): string {
        const con = extractConsonant(c);
        const vow = extractVowel(c);
        const conSound = consonantMapping.get(con);
        const vowSound = vowelMapping.get(vow);

        if (con != 0 && conSound == undefined) {
            return "red";
        }
        if (vow != 0 && vowSound == undefined) {
            return "red";
        }
        if (conSound != undefined && conSound.certainty != Certainty.Definite) {
            return "orange";
        }
        if (vowSound != undefined && vowSound.certainty != Certainty.Definite) {
            return "orange";
        }

        return "black";
    }
</script>
<svg xmlns="http://www.w3.org/2000/svg" width={2 * padding + charCount * charWidth} height={2 * padding + charHeight + textHeight} stroke-width="2" stroke="#000">
    <!-- Background -->
    <rect width="100%" height="100%" fill="#ddf" stroke="none" />
    <!-- Horizontal line, always present -->
    <line x1={padding} y1={padding + charHeight * .4} x2={padding + charCount * charWidth} y2={padding + charHeight * .4} />
    {#each chars as char, i}
        <!-- Top vertical left -->
        {#if char & 0b1000000000000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth}
                y1={padding + (charHeight * .4) * .3}
                y2={padding + charHeight * .4}
            />
        {/if}
        <!-- Top circle bottom left -->
        {#if char & 0b0100000000000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * .3}
                y2={padding + (charHeight * .4) * .6}
            />
        {/if}
        <!-- Top circle top left -->
        {#if char & 0b0010000000000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * .3}
                y2={padding}
            />
        {/if}
        <!-- Top vertical middle -->
        {#if char & 0b0001000000000000}
            <line
                x1={padding + i * charWidth + charWidth / 2}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding}
                y2={padding + (charHeight * .4) * .6}
            />
        {/if}
        <!-- Top circle bottom right -->
        {#if char & 0b0000100000000000}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * .3}
                y2={padding + (charHeight * .4) * .6}
            />
        {/if}
        <!-- Top circle top right -->
        {#if char & 0b0000010000000000}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * .3}
                y2={padding}
            />
        {/if}
        <!-- Top vertical right -->
        {#if char & 0b0000001000000000}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth}
                y1={padding + (charHeight * .4) * .3}
                y2={padding + charHeight * .4}
            />
        {/if}
        <!-- Top diacritic -->
        {#if char & 0b0000000100000000}
            <line
                x1={padding + i * charWidth + charWidth / 2}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + charHeight * .4}
                y2={padding + (charHeight * .4) * .6}
            />
        {/if}
        <!-- Bottom vertical left -->
        {#if char & 0b0000000010000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth}
                y1={padding + (charHeight * .4) * 1.4}
                y2={padding + (charHeight * .4) * 1.7}
            />
        {/if}
        <!-- Bottom circle bottom left -->
        {#if char & 0b0000000001000000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * 1.7}
                y2={padding + (charHeight * .4) * 2}
            />
        {/if}
        <!-- Bottom circle top left -->
        {#if char & 0b0000000000100000}
            <line
                x1={padding + i * charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * 1.7}
                y2={padding + (charHeight * .4) * 1.4}
            />
        {/if}
        <!-- Bottom vertical middle -->
        {#if char & 0b0000000000010000}
            <line
                x1={padding + i * charWidth + charWidth / 2}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + charHeight * .4 * 1.4}
                y2={padding + (charHeight * .4) * 2}
            />
        {/if}
        <!-- Bottom circle bottom right -->
        {#if char & 0b0000000000001000}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * 1.7}
                y2={padding + (charHeight * .4) * 2}
            />
        {/if}
        <!-- Bottom circle top right -->
        {#if char & 0b0000000000000100}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth / 2}
                y1={padding + (charHeight * .4) * 1.7}
                y2={padding + (charHeight * .4) * 1.4}
            />
        {/if}
        <!-- Bottom vertical right -->
        {#if char & 0b0000000000000010}
            <line
                x1={padding + i * charWidth + charWidth}
                x2={padding + i * charWidth + charWidth}
                y1={padding + charHeight * .4 * 1.4}
                y2={padding + (charHeight * .4) * 1.7}
            />
        {/if}
        <!-- Bottom diacritic -->
        {#if char & 0b0000000000000001}
            <circle
                cx={padding + i * charWidth + charWidth / 2}
                cy={padding + charHeight * .8 + 5}
                r="4"
                fill="none"
            />
        {/if}
        <text fill={charColor(char)} font-family="Helvetica" stroke-width="0.3" x={padding + charWidth * i} y={charHeight + padding * 2 + 10}>{charText(char)}</text>
    {/each}
</svg>
