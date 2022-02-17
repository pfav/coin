<script>
    import { router } from '../stores';

    export let routes;

    let route = router(routes);
</script>

{#if $route !== undefined}
    <svelte:component this={$route.component} {...$route.params}/>
{:else}
    <h2>Page not found</h2>
{/if}
