<script>
    export let tx;
    export let idx = 0;
</script>


<div class="collapse">
    <input type="checkbox" id="collapse-{tx.hash}" aria-hidden="true">
    
    <label for="collapse-{tx.hash}" aria-hidden="true">tx#{idx} | {tx.hash}</label>
    <div>
        <p>hash: <code>{tx.hash}</code></p>
        <!-- INPUT -->
        {#each tx.in as input, i}
            <p><b>in#{i}</b></p>
            {#if input['out-hash'] === undefined}
            <p>out-index: {input['out-index']} (coinbase)</p>
            {:else}
                <p>out-index: {input['out-index']}</p>
                <p>out-hash: <code>{input['out-hash']}</code></p>
                <p>signature: <code>{input['signature']}</code></p> 
            {/if}
        {/each}
    
        {#each tx.out as output, i}
            <p><b>out#{i}</b></p>
            <p>address: <code>{output['address']}</code></p>
            <p>amount: {output['amount']}</p>
        {/each}      
    </div>
</div>

