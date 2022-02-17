<script>
    import { onMount, afterUpdate } from "svelte";
    import Layout from "../components/Layout.svelte";
    import Block from "../components/Block.svelte";

    export let hash;

    let block = null;
    $: hs = hash.substr(0, 16);

    onMount(async () => {
        fetchBlock(hash);
    });

    afterUpdate(async () => {
        if (block && block.hash !== hash) {
            block = null;
            fetchBlock(hash);
        }
    });

    async function fetchBlock(hash) {
        let res = await fetch(`/api/blockchain/${hash}`);
        block = await res.json();
    }
</script>

<Layout title="Block {hs}">
    {#if block !== null}
        <Block {block} />
    {:else}
        <div class="spinner" />
    {/if}
</Layout>
