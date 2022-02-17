
import Wallet from './routes/Wallet.svelte';
import BlockExplorer from './routes/BlockExplorer.svelte';
import Block from './routes/Block.svelte';
import Peer from './routes/Peer.svelte';
import NotFound from './routes/NotFound.svelte';

const routes = [
    {
        name: '#/',  component: Wallet
    },
    {
        name: '#/block', component: BlockExplorer
    },
    {
        name: '#/block/:hash', component: Block
    },
    {
        name: '#/peers', component: Peer
    },
    {
        name: '#/404', component: NotFound
    }
];

export { routes }