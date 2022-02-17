<script>
    import Layout from '../components/Layout.svelte';
    import BranchSelect from '../components/BranchSelect.svelte';
    import Wallet from '../components/Wallet.svelte';
    import Payment from '../components/Payment.svelte';

    let branch = null;
    let wallet = null;
    
    async function lookupWallet() {
        const latest_hash = branch['latest-hash'];
        let res = await fetch(`/api/wallet/${latest_hash}`);
        wallet = await res.json();
    }

    async function reset() {
        branch = null;
        wallet = null;
    }
</script>

<Layout title='Wallet'>
    
    <div class='row'>
        <BranchSelect bind:branch on:selected={lookupWallet}/>
    </div>
    
    <div class='row'>
        <Payment {branch} on:payment={reset} />
        <Wallet {wallet} />

    </div>
    
</Layout>