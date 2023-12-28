<script lang="ts">
    import Word from "./Word.svelte";
</script>
<h2>Tunic Dictionary</h2>
<p>About the script: The top is 8 stroks, and the bottom is 8 strokes. Each word seems to be connected with a single horizontal line (like mongolian script). Question: Are the top and bottom mirrored? Near the end of the manual, it hints that stroke groups could be combined in a single character.</p>

<p><b>Stamina</b> (or maybe "SP")</p>
<p>On the stamina page, these symbols are repeated multiple times, implicitly around places that mean "low stamina", "full stamina", and the symbol by itself on some parts of the page.
<Word chars={[
0b0101000101001110,
0b1000110101011100,
]} />
<p>"Cards" (maybe), page 51, page 20</p>
<Word chars={[
0b0011110101001100,
0b0001000100100100,
0b0101000100010100,
]} />

<p><b>Cardinal directions</b>, page 34</p>
<p>Also in one of the holy cross challenges on a signpost.</p>
<p>West / Left</p>
<Word chars={[
0b1100100011001000,
]} />
<p>East / Right</p>
<Word chars={[
0b1011100111111001,
]} />
<p>South / Down</p>
<Word chars={[
0b0001100100111000,
]} />
<p>North / Up</p>
<Word chars={[
0b1110010010101100,
]} />
<p><b>Common repeats</b></p>
<p>I'm thinking this is the word "the". It's used in front of proper nouns like "West Garden", "Dark Tomb", "Door in the Mountains", "Fairies", "Holy Cross", "Cathedral"</p>
<Word chars={[
0b0011010100110100,
]} />

<p>Some more very common words:</p>
<p>This one in particular occurs before words "check-point", "civilization", "cathedral", "rudeling". And at the beginning of sentences / phrases. It's likely "a"</p>
<Word chars={[
0b0010010000000000,
]} />

<Word chars={[
0b0010010001001000,
]} />

<p>Wishing well, page 20</p>
<p>The first character is the same as "West". Last top right char is the same beginning as "stamina" and end of "cards". Might be "wish".</p>
<Word chars={[
0b1100100011001000,
0b0001000100010000,
0b0101000100010100,
]} />

<p>Buttons / pressing something like that, on pages 13 and 14. A big hint is that it's used beside both the "A" and "LT" buttons.</p>
<Word chars={[
0b0011010100000100,
0b0100100101011000,
0b0100000000100100,
]} />
