$(function() {
    $('#button1').on('click', function(event) {
	var formData = new FormData( $('#btForm')[0]);
	$.ajax({
	    url: 'bt',
	    method: 'post',
	    dataType: 'text',
	    data: formData,
	    processData: false,
	    contentType: false
	}).done(function( data ) {
	    $('#text1').val(data);
	}).fail(function() {
	    alert('error: some thing went wrong..');
	});
    });
 });
