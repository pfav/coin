<script>
    import { onMount } from 'svelte';
    import { setToast } from '../components/Toast.svelte';
    import Layout from '../components/Layout.svelte';
    import http from '../http';

    let peers = [];

    onMount(lookupPeers);

    async function lookupPeers() {
        let res = await fetch(`/api/peer`);
        peers = await res.json();
    }

    let ip;
    let port;
    async function addPeer() {
        let res = await http.POST('/api/peer', {ip, port})
        if (res.result !== "ok") {
            setToast(res.message || "something went wrong", 'error');
            return;
        } 
        setToast(`Added peer ${ip}:${port}`, 'success');
        lookupPeers();
    }
</script>

<Layout title='Peer'>
    <div class='row'>
        <div class='col-sm'>
            <form>
                <fieldset>
                  <legend>Add Peer</legend>
                  <label for="ip">Ip</label>
                  <input type="text" bind:value={ip} id='ip' placeholder="127.0.0.1"/>
                  <label for="port">Port</label>
                  <input type="number" bind:value={port} id='port' placeholder="8080"/>
                  
                  <button on:click={addPeer}>Add</button>
                </fieldset>
              </form>
        </div>
        <div class='col-sm'>
            <h4>connected peers</h4>
            <ul>
            {#each peers as peer (peer.ip + peer.port)}
                <li>{peer.ip}:{peer.port}</li>
            {/each}
            </ul>
        </div>
    </div>
</Layout>

<style>
    h4 {
        text-transform: uppercase;
        font-weight: 300;
    }
    ul {
        width: 100%;
        max-height: 380px;
        list-style-type: none;
        overflow: hidden; 
        overflow-y: scroll;
    }
</style>