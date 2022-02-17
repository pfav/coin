<script>
    export let wallet = null;

</script>

<div class='col-sm'>
    {#if wallet !== null}
        <div id='wallet' class='card fluid'>
            <h4 class='section dark'>account</h4>
            <div class='section' id='keys'>
                <p>private key:</p>
                <code>{wallet.private_key}</code>
                                
                <p>public key:</p>
                <code>{wallet.public_key}</code>
                
                <p>keyfile:</p>
                <code>{wallet.keyfile}</code>

                <p>balance:</p>
                <code>{(wallet.balance / 10000000000).toFixed(10)} C</code>
            </div>
        </div>
    {:else}
        <div class="spinner"></div>
    {/if}
</div>

<style>
    #wallet {
        border-radius: 10px;
        box-shadow: 5px 5px 5px #ccc;
    }
    h4 {
        text-transform: uppercase;
        font-weight: 300;
    }
    p {
        line-height: 44px;
        font-weight: 300;
        text-transform: uppercase;
        
    }
</style>