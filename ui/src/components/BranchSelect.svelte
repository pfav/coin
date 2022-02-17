<script>
	import { onMount, createEventDispatcher } from 'svelte';

    const dispatchBranch = createEventDispatcher();    

    export let branch = null;
    let branches = [];

    function selectBest() {
        if (branches.length == 0 )
            return null;

        let candidate = branches[0];

        for (let i = 1, len = branches.length; i < len; i++) {
    
            let current_diff = branches[i].branch['cumulative-difficulty'];
            let candidate_diff = candidate.branch['cumulative-difficulty']
            if (current_diff > candidate_diff){ 
                candidate = branches[i];
            } 
        }
        return candidate.branch;
    }

    onMount(lookupBranch);
    $: branch || lookupBranch();
    
    async function lookupBranch() {
        let res = await fetch('/api/blockchain/info');
        branches = await res.json();

        branch = selectBest();
        dispatchBranch('selected', branch);
    }

    function handleSelectBranch() {
        dispatchBranch('selected', branch);
    }
</script>


<div class='col-sm'>

    {#if branch !== null}
    <div id='slot-select fluid'>
        <label for='slot'>Slot: </label>
        <select id='slot' bind:value={branch} on:change={handleSelectBranch}>
        {#each branches as branch (branch.slot)}
            <option value={branch.branch}>
                { branch.branch['latest-hash'].substr(0, 16) } |
                { branch.branch['cumulative-difficulty'] }
            </option>
        {/each}
        </select>
    </div>
    {:else}
        <div class="spinner"></div>
    {/if}

</div>

<style>
    select {
        min-width: 600px;
        border-radius: 10px;
        box-shadow: 2px 2px 5px #ccc;
        text-transform: uppercase;
    }
    label {
        text-transform: uppercase;
        font-weight: 400;
    }
</style>