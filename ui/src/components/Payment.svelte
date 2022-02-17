<script>
    import http from '../http';
    import { setToast } from './Toast.svelte';
    import { createEventDispatcher } from 'svelte';


    export let branch = null;
    
    let address;
    let amount = 0;
    $: latest_hash = branch && branch['latest-hash'];

    const dispatch = createEventDispatcher();

    // payment
    async function handlePayment(e) {
        let res = await http.POST(`api/wallet/${latest_hash}/pay`, {address, amount});
        if (res.result == "ok") {
            setToast('submitted transaction', 'success');
            address = '';
            amount = 0;
            dispatch('payment', {});
        } else {
            setToast(res.message || 'something went wrong', 'error');
        }
    }
</script>



<div class='col-sm'>
    {#if branch !== null}
        <form id='send' on:submit|preventDefault={handlePayment}>
            <fieldset class="input-group vertical">
            <legend>Payment</legend>
            <label for="address">address</label>
            <input type="text" id="address" bind:value={address}/>

            <label for="amount">amount</label>
            <input type="number" id="amount" bind:value={amount} />
            
            <button id='pay'>Pay <span class="icon-credit"></span></button>
            
            <p id='amount-coin'>{(amount / 10000000000).toFixed(10)} C</p>

            </fieldset>
        </form>
    {:else}
        <div class="spinner"></div>
    {/if}
</div>    

<style>
    #send {
        border-radius: 5px;
        box-shadow: 5px 5px 5px #ccc;
    }
    #pay {
        width: 100%;
        font-size: 1.5em;
        margin-top: 50px;
    }
    label {
        text-transform: uppercase;
    }

    legend {
        text-transform: uppercase;
        font-weight: 300;
    }
    
    #amount-coin {
        text-align: right;
    }
</style>
