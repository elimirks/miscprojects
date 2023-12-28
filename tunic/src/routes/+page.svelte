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
<p>"Cards" (maybe), page 51, page 20</p>
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

<p>I assumed this was "an" since it's similar to "a", but it occurs before "the" on page 21.</p>
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

<p>After "a" on pages 17 and 10, when talking about keys. Could be "door" or "key", should be a noun.</p>
<Word chars={[
consonants[7] + vowels[6],
]} />

<p>On pages 17 and 10, when talking about keys. Could be "door" or "key". At the beginning of phrases, so could be something else too.</p>
<Word chars={[
consonants[21] + vowels[7],
]} />

<p><b>Shield</b></p>
<p>On pages 14 and 29, when talking about blocking. Could be "block" or "shield". But it's after "a" on page 29, so "a shield" fits better than "a block". It's also the word beside the shield icon on page 12.</p>
<Word chars={[
consonants[17] + vowels[6],
consonants[23],
consonants[6],
]} />
