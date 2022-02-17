<script>
    import Header from './Header.svelte'
    import Footer from './Footer.svelte';
    import Toast from './Toast.svelte';

    export let title;
</script>

<svelte:head>
	<title>Coin Admin - {title}</title>
</svelte:head>

<!-- Header -->
<Header />

<div class='cointainer'>

    <!-- Page Title -->
    <div class='row'>
        <div class='col-sm-12'>
            <h2>{title}</h2>
        </div>
    </div>

    <!-- content -->
    <slot></slot>
    
</div>

<!-- Toast -->
<Toast />

<!-- Footer -->
<Footer />

<style>
    h2 {
		margin: 0 auto;
		color: #13143c9e;
		text-transform: uppercase;
		font-size: 4em;
		font-weight: 100;
		text-align: center;
	}
</style>