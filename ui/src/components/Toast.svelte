<script context="module">
    import { toast as message } from '../stores';
    let timeout;

    export function setToast(msg, kind) {
        message.set({message: msg, kind: kind});
        clearTimeout(timeout);
        timeout = setTimeout(() => {
            message.set({message: null, kind: 'error'});
        }, 5000);
    }
</script>

<script>
    import { fly } from 'svelte/transition';
    import { toast } from '../stores';
</script>


{#if $toast.message !== null}
    <div id='toast' transition:fly="{{ x: 410, duration: 1200 }}" class='{$toast.kind}'>
        <p>{$toast.message}</p>
    </div>
{/if}


<style>
    #toast {
        position: absolute;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        right: 10px;
        width: 400px;
        height: 80px;
        top: 20px;
        z-index: 1200;
        border-radius: 5px;
        
        color: white;
        font-weight: 400;
        text-shadow: 1px 1px #666;
        box-shadow: 2px 2px 4px #ccc
    }
    .error {
        background-color: #e94a4a;
    }
    .success {
        background-color: rgb(21, 150, 21);
    }


</style>