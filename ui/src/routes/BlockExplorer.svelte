<script>
    import Layout from '../components/Layout.svelte';
    import BranchSelect from '../components/BranchSelect.svelte';
    import BranchInfo from '../components/BranchInfo.svelte';
    
    // export let params = {};
    
    let branch = null;
    let hashes = [];

    async function handleSelected() {
        let latest_hash = branch['latest-hash'];
        let res = await fetch(`/api/blockchain/?start=${latest_hash}&count=100`);
        hashes = await res.json();
    }
</script>

<Layout title="Block Explorer">
    <div class='row'>
        <BranchSelect bind:branch on:selected={handleSelected} />
    </div>
    <div class='row'>
        <BranchInfo bind:branch /> 
        <div class='col-sm'>
            <h4>Blockchain</h4>
            <ul>
                {#each hashes as hash, i (hash) }
                    <li><a href="#/block/{hash}">{i} : {hash.substr(0, 16)}</a></li>
                {/each}
            </ul>
        </div>
    </div>
</Layout>

<style>
    ul {
        width: 100%;
        max-height: 380px;
        list-style-type: none;
        overflow: hidden; 
        overflow-y: scroll;
    }
</style>