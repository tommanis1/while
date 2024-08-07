// A javscript frontend for rendering a While configuration.

// Split page in two, top 2/3 for displaying the current config, bottom 1/3 for the history
function init() {
    let body = document.body;

    let configContainer = document.createElement('div');
    configContainer.id = 'current-config';
    configContainer.style.height = '66vh';
    let historyContainer = document.createElement('div');
    historyContainer.id = 'history';
    historyContainer.style.height = '33vh';

    body.appendChild(configContainer);
    body.appendChild(historyContainer);

    otherEntitiesDisplay = document.createElement('div');
    otherEntitiesDisplay.id = 'other-entities-display';
    otherEntitiesDisplay.style.height = '33%';

    threadsDisplay = document.createElement('div');
    threadsDisplay.id = 'thread-display';
    threadsDisplay.style.height = '66%';
    
    configContainer.appendChild(otherEntitiesDisplay);
    configContainer.appendChild(threadsDisplay);

}

init();

// Display the non-thread related entities separate from the threads.
    // The top 1/3 of the current config container should be set to display content.       
function create_other_entities_display(content) {      
    let otherEntitiesDisplay = document.getElementById('other-entities-display');
   
    otherEntitiesDisplay.innerHTML = content;
}

// Create a collapsable menu that list all possible threads. Clicking a thread toggles its visibility
// list_threads(ids) {
//     // In the current config container, add a collabsable menu listing each id in ids. The onclick for each item sets the visibility of the html element with the same id.


// }


function create_thread_display(thread_id, content, callback_id, isCurrent) {
    // create a movable box for each thread using interact.js, with the visibilty set to hidden by default and toggled by the element created in list_threads. If isCurrent is true the box is outlined to denote that this is the current thread

    // search for element with id thread_id in threadsDisplay
}

