module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;

//logic [ 4:0] state;
logic [31:0] hout[num_nonces];

// OUR Addition
logic [31:0] w[16];			//16 32-bit blocks
logic [31:0] message[32];	//32 32-bit blocks
logic [31:0] wt;				//current 32-bit block
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, h;
logic [ 7:0] i, j;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;
logic [3:0] ct;

// BIT addition
integer NUM_OF_WORDS = 19;
logic [31:0] count = 32'h00000000;
logic [2:0] phase;
logic [15:0] bit_offset;




// FSM state variables 
enum logic [4:0] {IDLE, READ, BLOCK, COMPUTE, WRITE, WAIT, SET, NULL} state;





parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


	assign num_blocks = determine_num_blocks(NUM_OF_WORDS); 
	//assign tstep = (i - 1);
	
	// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
	
	// Function to determine number of blocks in memory to fetch
	function logic [15:0] determine_num_blocks(input logic [31:0] size);
		begin			
			if(size <= 16) 	//16*32 = 512
				determine_num_blocks = 1;
			else 
				determine_num_blocks = 2;
		end
	endfunction
	
	
	// SHA256 hash round
	function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
												input logic [7:0] t);
		 logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
		begin
			S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
			ch = (e & f) ^ ((~e) & g); 
			t1 = h + S1 + ch + k[t] + w;
			S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
			maj = (a & b) ^ (a & c) ^ (b & c);
			t2 = S0 + maj;
			sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
		end
	endfunction
	
	
	// Generate request to memory
	// for reading from memory to get original message
	// for writing final computed has value
	assign mem_clk = clk;
	assign mem_addr = cur_addr + offset;
	assign mem_we = cur_we;
	assign mem_write_data = cur_write_data;
	
	
	// Right Rotation Example : right rotate input x by r
	// Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
	// lets say r = 4
	// x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777 
	// x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
	// final right rotate expression is = (x >> r) | (x << (32-r));
	// (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
	// final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
	// Right rotation function
	function logic [31:0] rightrotate(input logic [31:0] x,
												 input logic [ 7:0] r);
		begin
			rightrotate = ((x >> r) | (x << (32 - r)));
		end
	endfunction
	
	
	//word expansion from slides
	function logic [31:0] word_expansion(int t);
		logic [31:0] temp_s0;
		logic [31:0] temp_s1;
		begin
			if(t < 16) begin
				return w[t];
			end
			else begin
				temp_s0 = rightrotate(w[t-15], 7) ^ rightrotate(w[t-15], 18) ^ (w[t-15] >> 3);
				temp_s1 = rightrotate(w[t-2], 17) ^ rightrotate(w[t-2], 19) ^ (w[t-2] >> 10);
				return  w[t-16] + temp_s0 + w[t-7] + temp_s1;
			end
		end
	endfunction
	
	

always_ff @(posedge clk, negedge reset_n) begin
	  if (!reset_n) begin
		 ct <= 0;
		 cur_we <= 1'b0;
		 state <= IDLE;
		 count <= 32'h00000000;
		 phase <= 1;
	  end 
	  else case (state)
		IDLE: begin
			if(start) begin
						phase <= 1;
						i <= 0;
						j <= 0;
						offset <= 0;	
						bit_offset <= 0;
						
						a <= 32'h6a09e667;
						h0 <= 32'h6a09e667;

						b <= 32'hbb67ae85;
						h1 <= 32'hbb67ae85;

						c <= 32'h3c6ef372;
						h2 <= 32'h3c6ef372;

						d <= 32'ha54ff53a;
						h3 <= 32'ha54ff53a;

						e <= 32'h510e527f;
						h4 <= 32'h510e527f;

						f <= 32'h9b05688c;
						h5 <= 32'h9b05688c;

						g <= 32'h1f83d9ab;
						h6 <= 32'h1f83d9ab;

						h <= 32'h5be0cd19;						
						h7 <= 32'h5be0cd19;
						
						cur_we <= 0;	//read from memory
						cur_addr <= message_addr; //memory address = address of message
						cur_write_data <= 0;
						state <= WAIT;
						count <= 32'h00000000;
					end
		end
		READ: begin
			if(offset < NUM_OF_WORDS) begin 	//if all words have NOT been read yet
				message[offset] <= mem_read_data;
				state <= WAIT;				//give 1 cycle wait time for data to be read from memory
				offset <= offset + 1;
			end
			else begin		
				for(int i = 0; i < 13; i++) begin
					if(i == 0) 
						message[19+i] <= count; //1 bit followed by 0's padding
					else if (i == 1)
						message[19+i] <= 32'h80000000;
					else if(i == 12)
						message[19+i] <= 32'd640;	//message length size since there are 19 words
					else 
						message[19+i] <= 0;	//rest of 0's padding
				end
				
				state <= BLOCK;	//all data read, move on to next step 
				offset <= 0;	//reset offset
			end	
		end
		WAIT: begin
			if(ct < 4) begin
				ct <= ct + 1;
				state <= WAIT;
			end
			else
				state <= READ;
		end
		BLOCK: begin
			// Fetch message in 512-bit block size
			// For each of 512-bit block initiate hash value computation
			if(i < num_blocks) begin
				state <= COMPUTE;
				i <= i + 1;
				for(int t = 0; t < 16; t++) begin
					w[t] <= message[(i << 4) + t]; //from w[0] to w[15], since 512 breaks up into 16 32-bit chunks
				end
			end
			else begin
				state <= SET;	
			end
		end
		COMPUTE: begin
			// 64 processing rounds steps for 512-bit block 
			if (j < 64) begin						
				//word expansion
				w[16-1] <= word_expansion(16);
				for(int t = 1; t < 16; t++) begin
					w[t-1] <= w[t];
				end
				//compression
				{a, b, c, d, e, f, g, h} <= sha256_op(a, b, c, d, e, f, g, h, w[16-16], j);
				state <= COMPUTE;
				j <= j + 1;
			end
			else begin		//64 rounds done, save a-h, h0-h7, go back to BLOCK
				j <= 0;
							
				a <= h0 + a;
				h0 <= h0 + a;

				b <= h1 + b;
				h1 <= h1 + b;

				c <= h2 + c;
				h2 <= h2 + c;

				d <= h3 + d;
				h3 <= h3 + d;

				e <= h4 + e;
				h4 <= h4 + e;

				f <= h5 + f;
				h5 <= h5 + f;

				g <= h6 + g;
				h6 <= h6 + g;

				h <= h7 + h;
				h7 <= h7 + h;
							
				state <= BLOCK; //finished computing 1 block, go back to BLOCK 
			end
		end
		SET: begin		//preps to save the hash values
			if (count == num_nonces) begin
				state <= IDLE;
			end
			else if (phase == 1) begin
				state <= BLOCK;
				phase <= 3;
				
				message[0] <= h0; 
				message[1] <= h1; 
				message[2] <= h2; 
				message[3] <= h3; 
				message[4] <= h4; 
				message[5] <= h5; 
				message[6] <= h6;
				message[7] <= h7; 	
				message[8] <= 32'h80000000;
				
				for(int i = 0; i < 7; i++) begin
					if(i == 6) 
						message[9+i] <= 32'd256; 
					else begin
						message[9+i] <= 32'h00000000;
					end
				end
					
				NUM_OF_WORDS = 16;
				a <= 32'h6a09e667;
				h0 <= 32'h6a09e667;

				b <= 32'hbb67ae85;
				h1 <= 32'hbb67ae85;

				c <= 32'h3c6ef372;
				h2 <= 32'h3c6ef372;

				d <= 32'ha54ff53a;
				h3 <= 32'ha54ff53a;

				e <= 32'h510e527f;
				h4 <= 32'h510e527f;

				f <= 32'h9b05688c;
				h5 <= 32'h9b05688c;

				g <= 32'h1f83d9ab;
				h6 <= 32'h1f83d9ab;

				h <= 32'h5be0cd19;						
				h7 <= 32'h5be0cd19;
				
				offset <= 0;
				i <= 0;
				j <= 0;	
				
			end
			else begin			
				cur_addr <= output_addr + bit_offset;
				
				cur_write_data <= h0;
				cur_we <= 1;
				state <= NULL;
				count <= count + 1;
			end
			
		end
		NULL: begin
				bit_offset <= bit_offset + 1;
				offset <= 0;
				phase <= 1;
				i <= 0;
				j <= 0;	
						
				a <= 32'h6a09e667;
				h0 <= 32'h6a09e667;

				b <= 32'hbb67ae85;
				h1 <= 32'hbb67ae85;

				c <= 32'h3c6ef372;
				h2 <= 32'h3c6ef372;

				d <= 32'ha54ff53a;
				h3 <= 32'ha54ff53a;

				e <= 32'h510e527f;
				h4 <= 32'h510e527f;

				f <= 32'h9b05688c;
				h5 <= 32'h9b05688c;

				g <= 32'h1f83d9ab;
				h6 <= 32'h1f83d9ab;

				h <= 32'h5be0cd19;						
				h7 <= 32'h5be0cd19;
						
				cur_we <= 0;	//read from memory
				cur_addr <= message_addr; //memory address = address of message
				cur_write_data <= 0;
				state <= WAIT;		
				NUM_OF_WORDS = 19;
		end
		
		
		
		endcase
		
		
		
		
end
		
		

assign done = (state == IDLE);
	
	


endmodule
