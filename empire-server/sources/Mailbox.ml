open Empire ;;

let post_message game message = Queue.add message game.g_mailbox ;;

let have_message game = not (Queue.is_empty game.g_mailbox) ;;

let read_message game = Queue.take game.g_mailbox ;;
