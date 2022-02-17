<script>
    export let branch = null;
</script>


<div class="col-sm">
    {#if branch !== null} 
    <div class='card fluid'>
        <h4 class='section dark'>Info</h4>
    
        <div class='section'>
            <p>latest-hash: 
                <a href="#/block/{ branch['latest-hash'] }" >
                    { branch['latest-hash'] }
                </a>
            </p>
            <p>current difficulty: {branch['current-difficulty']}</p> 
            <p>cumulative-difficulty: {branch['cumulative-difficulty']}</p>
            <p>length: {branch['length']}</p>
            <p>coinbase: {branch['coinbase']}</p>
            <p>supply: {branch['supply']}</p>
        </div>
    </div>
    {:else}
        <div class="spinner"></div>
    {/if}
</div>

<style>
    h4 {
        text-transform: uppercase;
        font-weight: 300;
    }
    p {
        text-transform: uppercase;
        line-height: 44px;
        border-bottom: 1px solid #ccc;
    }
    
</style>