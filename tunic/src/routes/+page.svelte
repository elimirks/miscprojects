<script lang="ts">
    import Word from "./Word.svelte";
    import { vowels, consonants, flip } from "$lib";
</script>
<h1>Tunic Dictionary</h1>
<p>About the script: The top is 8 stroks, and the bottom is 8 strokes. Each word seems to be connected with a single horizontal line (like mongolian script). Question: Are the top and bottom mirrored? Near the end of the manual, it hints that stroke groups could be combined in a single character. A set of outside and internal edges can be combined, as shown in page 54 (and elsewhere). I looked up a hint: They're consonant-vowel combinations, and a circle at the bottom means to flip the pronunciation of the vowel and consonants.</p>

<h1>Character Construction</h1>
Take a vowel sound, take a consonant sound, and glue 'em together. Add a circle at the bottom to flip the sound of the consonant and vowel (otherwise consonant comes first).
<h2>Vowel Sounds</h2>
{#each vowels as char, i}
    <Word chars={[char]} />
    {#if (i + 1) % 6 === 0}
        <br />
    {/if}
{/each}
<h2>Consonant Sounds</h2>
{#each consonants as char, i}
    <Word chars={[char]} />
    {#if (i + 1) % 6 === 0}
        <br />
    {/if}
{/each}
<h2>Consonant/Vowel Flip Marker</h2>
    <Word chars={[flip]} />
<h1>Words</h1>

<p><b>Stamina</b> (or maybe "SP")</p>
<p>On the stamina page, these symbols are repeated multiple times, implicitly around places that mean "low stamina", "full stamina", and the symbol by itself on some parts of the page.</p>
<Word chars={[
consonants[12] + vowels[2],
consonants[8] + vowels[8],
]} />
<p><b>Cards</b></p>
<p>Page 51, page 20. Similar first sound to "key", combined with "ar".</p>
<Word chars={[
consonants[7] + vowels[10],
consonants[6],
consonants[16],
]} />

<p><b>Cardinal directions</b>, page 34</p>
<p>Also in one of the holy cross challenges on a signpost. I'm not sure if these are full words, or abbreviations.</p>
<p>West / Left</p>
<Word chars={[
consonants[22] + vowels[3],
]} />
<p>East / Right</p>
<Word chars={[
consonants[15] + vowels[6] + flip,
]} />
<p>South / Down</p>
<Word chars={[
consonants[15] + vowels[15],
]} />
<p>North / Up</p>
<Word chars={[
consonants[1] + vowels[11],
]} />
<p><b>Common repeats</b></p>
<p>I'm thinking this is the word "the". It's used in front of proper nouns like "West Garden", "Dark Tomb", "Door in the Mountains", "Fairies", "Holy Cross", "Cathedral"</p>
<Word chars={[
consonants[14] + vowels[5],
]} />

<p>Some more very common words:</p>
<p>This one in particular occurs before words "check-point", "civilization", "cathedral", "rudeling". And at the beginning of sentences / phrases. It's likely "a"</p>
<Word chars={[
vowels[5]
]} />

<p><b>are</b></p>
<p>Occurs in "cards" as well as by itself. On page 21 it appears before "the", so "are" makes sens.</p>
<Word chars={[
vowels[10]
]} />

<p>Wishing well, page 20</p>
<p>The first character is the same as "West". Last top right char is the same beginning as "stamina" and end of "cards". Might be "wish".</p>
<Word chars={[
consonants[22] + vowels[3],
consonants[23],
consonants[16],
]} />

<p>Buttons / pressing something like that, on pages 13 and 14. A big hint is that it's used beside both the "A" and "LT" buttons.</p>
<Word chars={[
consonants[4] + vowels[5],
consonants[5] + vowels[2],
consonants[1],
]} />

<p><b>Key</b></p>
<p>After "a" on pages 17 and 10, when talking about keys. We know the "k" sound from "cards" and "ee" from East.</p>
<Word chars={[
consonants[7] + vowels[6],
]} />

<p>On pages 17 and 10, when talking about keys. At the beginning of phrases.</p>
<Word chars={[
consonants[21] + vowels[7],
]} />

<p>Page 41 title, also used on page 42 for at alternate phrase for "ghost"</p>
<p>Word 1:</p>
<Word chars={[
consonants[11] + vowels[10],
]} />
<p>Word 2:</p>
<Word chars={[
consonants[17] + vowels[11],
]} />

<p>On page 41 it's after "the":</p>

The
<Word chars={[
consonants[11] + vowels[10],
]} />
<Word chars={[
consonants[17] + vowels[11],
]} />

<p>On page 42 it's after something else. In total, it's another word for "ghost"</p>
<Word chars={[
consonants[1] + vowels[2] + flip,
]} />
<Word chars={[
consonants[11] + vowels[10],
]} />
<Word chars={[
consonants[17] + vowels[11],
]} />

<p>On page 10, beside the old house checkbox and key:</p>
<p>Probably "you will need a key"</p>
<Word chars={[
consonants[21] + vowels[7],
]} />
<Word chars={[
consonants[22] + vowels[2],
consonants[23],
]} />
<Word chars={[
consonants[1] + vowels[6],
consonants[6],
]} />
<Word chars={[
vowels[5],
]} />
<Word chars={[
consonants[7] + vowels[6],
]} />

<p>On page 10, beside the dark tomb checkbox and lantern icon:</p>
<Word chars={[
consonants[6] + vowels[10],
consonants[7],
]} />
!
<Word chars={[
consonants[1] + vowels[6],
consonants[6],
]} />
<Word chars={[
vowels[5],
]} />
<Word chars={[
consonants[23] + vowels[13],
consonants[5],
]} />

<p>On page 10, speech bubble referring to the overworld map page:</p>
<Word chars={[
consonants[23] + vowels[1],
consonants[15],
consonants[5],
]} />
?
<Word chars={[
consonants[15] + vowels[6],
]} />
p. 28
<h3>Page 11</h3>
<p>A phrase pointing to the yellow teleporter square:</p>

<Word chars={[
consonants[15],
consonants[5],
consonants[20] + vowels[12],
consonants[1],
consonants[9],
]} />
<Word chars={[
consonants[8] + vowels[12],
consonants[5],
]} />
<Word chars={[
consonants[5] + vowels[7],
]} />
<Word chars={[
consonants[14] + vowels[5],
]} />
<Word chars={[
consonants[11] + vowels[10],
]} />
<Word chars={[
consonants[17] + vowels[11],
]} />
<h3>Page 12</h3>
<p>The controls page, the title is:</p>
<Word chars={[
consonants[7] + vowels[5],
consonants[1],
consonants[5],
consonants[20] + vowels[16],
consonants[23],
consonants[16],
]} />

<p>On pages 14 and 29, when talking about blocking. Could be "block" or "shield". But it's after "a" on page 29, so "a shield" fits better than "a block". It's also the word beside the shield icon on page 12.</p>
<Word chars={[
consonants[17] + vowels[6],
consonants[23],
consonants[6],
]} />

<p>Pointing to the movement joystick:</p>
<Word chars={[
consonants[0] + vowels[7],
consonants[12],
]} />

<p>Health potion icon:</p>
<Word chars={[
consonants[3] + vowels[16],
consonants[17] + vowels[5],
consonants[1],
]} />

<p>Sword / item slot icons:</p>
<Word chars={[
consonants[21] + vowels[7],
consonants[16],
]} />
<Word chars={[
consonants[5] + vowels[13] + flip,
consonants[0] + vowels[3] + flip,
]} />

<p>Pointing to left trigger (lock button):</p>
<p>On page 13, it talks about LT as the "focus" button</p>
<Word chars={[
consonants[11] + vowels[16],
consonants[7] + vowels[5],
consonants[15],
]} />

<h3>Page 54</h3>
<p>"For additional support & secrets"</p>

<p>1.</p>
<Word chars={[
consonants[11] + vowels[13],
consonants[1],
consonants[6],
]} />
<Word chars={[
consonants[15] + vowels[5],
consonants[0],
]} />
<Word chars={[
consonants[20] + vowels[17],
]} />
<Word chars={[
consonants[8] + vowels[16],
consonants[23],
consonants[6] + vowels[2],
consonants[1],
]} />
<Word chars={[
consonants[15],
consonants[5] + vowels[1],
consonants[10] + vowels[7],
consonants[16],
]} />
<p>2.</p>
<Word chars={[
consonants[5],
consonants[20] + vowels[5],
consonants[12] + vowels[8],
consonants[15],
]} />
<Word chars={[
consonants[14] + vowels[5],
]} />
<Word chars={[
consonants[8],
consonants[23] + vowels[16],
]} />
<Word chars={[
consonants[5] + vowels[7],
]} />
<Word chars={[
consonants[12] + vowels[2],
consonants[16] + vowels[2],
consonants[5],
]} />
12
<Word chars={[
consonants[15],
consonants[5],
consonants[20] + vowels[12],
consonants[1],
consonants[9],
]} />
<Word chars={[
consonants[4] + vowels[6],
consonants[2] + vowels[2] + flip,
consonants[16],
]} />

<p>3.</p>
"
<Word chars={[
consonants[1] + vowels[5] + flip,
]} />
<Word chars={[
consonants[15] + vowels[2],
consonants[2],
]} />
"

<Word chars={[
consonants[5] + vowels[7],
]} />
<Word chars={[
consonants[14] + vowels[3],
consonants[0],
]} />
<Word chars={[
consonants[14] + vowels[5],
]} />
<Word chars={[
consonants[8],
consonants[20] + vowels[12],
consonants[5] + vowels[2],
consonants[15],
consonants[5],
]} />
<Word chars={[
consonants[15] + vowels[1],
consonants[2],
]} />
,
<Word chars={[
consonants[14] + vowels[5],
]} />
<Word chars={[
consonants[15] + vowels[1],
consonants[2],
]} />
<Word chars={[
consonants[12] + vowels[5] + flip,
]} />
<Word chars={[
consonants[14] + vowels[5],
]} />
<Word chars={[
consonants[8] + vowels[16],
consonants[23],
consonants[6] + vowels[2],
consonants[1],
]} />
<Word chars={[
consonants[3] + vowels[0],
consonants[13],
]} />
,
<Word chars={[
consonants[16] + vowels[0] + flip,
]} />
<Word chars={[
consonants[15] + vowels[6],
consonants[1],
]} />
<Word chars={[
consonants[11],
consonants[20] + vowels[5],
consonants[0],
]} />
<Word chars={[
consonants[22] + vowels[2],
consonants[13] + vowels[2],
consonants[1],
]} />
