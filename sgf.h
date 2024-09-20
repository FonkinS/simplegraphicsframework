#ifndef SGF_H
#define SGF_H

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#define GL_SILENCE_DEPRECATION
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

#include <OpenGL/gl3.h>


/* -------------------- PNG LOADING ---------------------------------- */
enum COLOR_TYPES {
    PALETTE = 1,
    COLOR = 2,
    ALPHA = 4
};

enum BTYPES {
    NO_COMPRESSION,
    FIXED_HUFFMAN,
    DYNAMIC_HUFFMAN,
    ERROR
};

typedef unsigned char byte;

typedef struct {
    unsigned int length;
    char name[5];
    byte* data;
    int crc;
    byte crc_valid;
} chunk;

unsigned long crc(char *buf1, byte* buf2, int len) {
    return 0;
}

chunk read_chunk(FILE* f) {
    chunk c;
    c.length = (fgetc(f) << 24) + 
                (fgetc(f) << 16) + 
                (fgetc(f) << 8) + 
                fgetc(f);

    
    c.name[0] = fgetc(f);
    c.name[1] = fgetc(f);
    c.name[2] = fgetc(f);
    c.name[3] = fgetc(f);
    c.name[4] = 0;

    c.data = (byte*) malloc(c.length * sizeof(byte));
    for (unsigned int i = 0; i < c.length; i++) {
        c.data[i] = fgetc(f);
    }

    c.crc = (fgetc(f) << 24) + 
            (fgetc(f) << 16) + 
            (fgetc(f) << 8) + 
            fgetc(f);
    c.crc_valid = (c.crc) == (crc(c.name, c.data, c.length+4));

    return c;
}

int bits(byte* d, unsigned long long *i, int l) {
    int o = 0;
    for (int j = 0; j < l; j++) {
        o += ((d[(int)(*i/8)] >> (*i%8)) % 2) << (j);
        (*i) = (*i) + 1;
    }
    return o;
}

int bit(byte* d, unsigned long long *i) {
    (*i) = (*i) + 1;
    return ((d[(int)((*i-1)/8)] >> ((*i-1)%8)) % 2);
}

byte reverse(byte input, int size) {
    byte output = 0;
    for (int i = 0; i < size; i++) {
        output <<= 1;
        output += (input >> i) % 2;
    }
    return output;
}


void create_tree(byte *lengths, int *tree, int size) {
    bzero(tree, size * 4);
    int bl_count[16];
    bzero(bl_count, 16*4);
    for (int i = 0; i < size; i++) bl_count[lengths[i]] += 1;
    int code = 0;
    bl_count[0] = 0;
    int next_code[17];
    bzero(next_code, 17*4);
    for (int b = 1; b <= 16; b++) {
        code = (code + bl_count[b-1]) << 1;
        next_code[b] = code;
    }
    for (int n = 0; n < size; n++) {
        int len = lengths[n];
        if (len != 0) {
            tree[n] = next_code[len];
            next_code[len] += 1;
        }
    }
    
}


void canonise_tree(int* tree, byte* lengths, short *indices, short size) {
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size-1; j++) {
            if (lengths[j] > lengths[j+1]) {
                int t = tree[j];
                tree[j] = tree[j+1];
                tree[j+1] = t;
                t = lengths[j];
                lengths[j] = lengths[j+1];
                lengths[j+1] = t;
                t = indices[j];
                indices[j] = indices[j+1];
                indices[j+1] = t;
            }  
        }
    }
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size-1; j++) {
            if (lengths[j] == lengths[j+1]) {
                if (tree[j] > tree[j+1]) {
                    int t = tree[j];
                    tree[j] = tree[j+1];
                    tree[j+1] = t;
                    t = indices[j];
                    indices[j] = indices[j+1];
                    indices[j+1] = t;

                }
            }  
        }
    }
}


unsigned int decompress_chunk(byte* data, byte** output, unsigned long long *b,unsigned long long byteIndex, int *BFINAL) {
    (*BFINAL) = bit(data, b);
    int BTYPE = bits(data, b, 2);

    byte *lit_lengths;
    byte *dist_lengths;
    int lit_amount = 0;
    int dist_amount = 0;
    if (BTYPE == NO_COMPRESSION) {
        if ((*b) % 8 != 0) (*b) += 8-((*b)%8);
        short len = bits(data, b, 16);
        short nlen = bits(data, b, 16);
        (*output) = (byte*) realloc((*output), byteIndex+len * sizeof(byte));
        unsigned long long end = byteIndex+len;
        for (int i = 0; i < len; i++) {
            (*output)[byteIndex++] = bits(data, b, 8);
        }
        return byteIndex;
    } else if (BTYPE == FIXED_HUFFMAN) {
        lit_lengths = (byte*) calloc(288, sizeof(byte));
        lit_amount = 288;
        dist_lengths = (byte*) calloc(32, sizeof(byte));
        dist_amount = 32;
        for (int i = 0; i < 144; i++) lit_lengths[i] = 8; 
        for (int i = 144; i < 256; i++) lit_lengths[i] = 9; 
        for (int i = 256; i < 280; i++) lit_lengths[i] = 7; 
        for (int i = 280; i < 288; i++) lit_lengths[i] = 8; 
        for (int i = 0; i < 32; i++) dist_lengths[i] = 5;
    } else if (BTYPE == DYNAMIC_HUFFMAN) {
        int HLIT = bits(data, b, 5);
        int HDIST = bits(data, b, 5);
        int HCLEN = bits(data, b, 4);
        lit_lengths = (byte*) calloc(HLIT+257, sizeof(byte));
        lit_amount = HLIT + 257;
        dist_lengths = (byte*) calloc(HDIST+1, sizeof(byte));
        dist_amount = HDIST + 1;

        byte code_symbols[] = {16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};
        byte code_lengths[19];
        int i;
        for (i = 0; i < HCLEN+4; i++) {
            code_lengths[code_symbols[i]] = bits(data, b, 3);
            if (code_lengths[code_symbols[i]]==0)continue;
        } for (; i < 19; i++) {
            code_lengths[code_symbols[i]] = 0;
        }
        int code_tree[19];
        short code_indices[19];
        for (i = 0; i < 19; i++) code_indices[i] = i;
        create_tree(code_lengths, code_tree, 19);
        canonise_tree(code_tree, code_lengths, code_indices, 19);

        byte code_count[16];
        bzero(code_count, 16);
        for (i = 0; i < 19; i++) code_count[code_lengths[i]] += 1;

        i = 0;
        int last = 0;
        while (i < HLIT + 257) {
            int code = 0;
            int first = 0;
            int index = code_count[0];
            for (int len = 1; len <= 16; len++) {
                code |= bit(data, b);
                int count = code_count[len];
                if (code - count < first) {
                    code = code_indices[index + (code - first)];
                    break;
                }
                index += count;
                first += count;
                first <<= 1;
                code <<= 1;
            }
            if (code < 16) {
                lit_lengths[i] = code;
                last = code;
                i++;
            } else if (code == 16) {
                int repeat = bits(data, b, 2) + 3;
                for (int j = 0; j < repeat; j++) {
                    lit_lengths[i] = last;
                    i++;
                }
            } else if (code == 17) {
                int repeat = bits(data, b, 3) + 3;
                for (int j = 0; j < repeat; j++) {
                    lit_lengths[i] = 0;
                    i++;
                }
            } else if (code == 18) {
                int repeat = bits(data, b, 7) + 11;
                for (int j = 0; j < repeat; j++) {
                    lit_lengths[i] = 0;
                    i++;
                }
            }

        } 
        i = 0;
        while (i < HDIST + 1) {
            int code = 0;
            int first = 0;
            int index = code_count[0];
            for (int len = 1; len <= 16; len++) {
                code |= bit(data, b);
                int count = code_count[len];
                if (code - count < first) {
                    code = code_indices[index + (code - first)];
                    break;
                }
                index += count;
                first += count;
                first <<= 1;
                code <<= 1;
            }
            if (code < 16) {
                dist_lengths[i] = code;
                last = code;
                i++;
            } else if (code == 16) {
                int repeat = bits(data, b, 2) + 3;
                for (int j = 0; j < repeat; j++) {
                    dist_lengths[i] = last;
                    i++;
                }
            } else if (code == 17) {
                int repeat = bits(data, b, 3) + 3;
                for (int j = 0; j < repeat; j++) {
                    dist_lengths[i] = 0;
                    i++;
                }
            } else if (code == 18) {
                int repeat = bits(data, b, 7) + 11;
                for (int j = 0; j < repeat; j++) {
                    dist_lengths[i] = 0;
                    i++;
                }
            }

        }
        
    } else {
        printf("ERROR LINE 100\n");
        return 0;
    }

    int* lit_tree = (int*) calloc(lit_amount, sizeof(int));
    short* lit_indices = (short*) calloc(lit_amount, sizeof(short));
    for (int i = 0; i < lit_amount; i++) lit_indices[i] = i;
    int lit_count[16];
    bzero(lit_count, 16*4);
    for (int i = 0; i < lit_amount; i++) lit_count[lit_indices[lit_lengths[i]]]++;
    create_tree(lit_lengths, lit_tree, lit_amount);
    canonise_tree(lit_tree, lit_lengths, lit_indices, (short) lit_amount);

    int* dist_tree = (int*) calloc(dist_amount, sizeof(int));
    short* dist_indices = (short*) calloc(dist_amount, sizeof(short));
    for (int i = 0; i < dist_amount; i++) dist_indices[i] = i;
    int dist_count[16];
    bzero(dist_count, 16*4);
    for (int i = 0; i < dist_amount; i++) dist_count[dist_lengths[i]]++;
    create_tree(dist_lengths, dist_tree, dist_amount);
    canonise_tree(dist_tree, dist_lengths, dist_indices, (short) dist_amount);

    int lengths_table[] = {
        3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,
        43,51,59,67,83,99,115,131,163,195,227,258,259
    };
    int distances_table[] = {
        1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,
        385,513,769,1025,1537,2049,3073,4097,6145,8193,
        12289,16385,24577
    };

    //unsigned long long byteIndex = 0;
    unsigned long long totalLength = byteIndex+256;
    

    while (1) {
        int code = 0;
        int first = 0;
        int index = lit_count[0];
        for (int len = 1; len <= 16; len++) {
            code |= bit(data, b);
            int count = lit_count[len];
            if (code - count < first) {
                code = lit_indices[index + (code - first)];
                break;
            }
            index += count;
            first += count;
            first <<= 1;
            code <<= 1;
        }
        if (code < 256) {
            (*output)[byteIndex++] = code;
            if (byteIndex >= totalLength) {
                totalLength += 256;
                (*output) = (byte*) realloc(*output, totalLength * sizeof(byte));
            }
        } else if (code == 256) {
            break;
        } else {
            int min_length = lengths_table[code-257];
            int extra_bits = 0;
            if (code > 264) extra_bits = (int)((code - 261) / 4);
            if (code == 285) extra_bits = 0;
            int length = min_length + bits(data, b, extra_bits);

            int dcode = 0;
            int dfirst = 0;
            int dindex = dist_count[0];
            for (int len = 1; len <= 16; len++) {
                dcode |= bit(data, b);
                int count = dist_count[len];
                if (dcode - count < dfirst) {
                    dcode = dist_indices[dindex + (dcode - dfirst)];
                    break;
                }
                dindex += count;
                dfirst += count;
                dfirst <<= 1;
                dcode <<= 1;
            }

            int min_distance = distances_table[dcode];
            int nextra_bits = 0;
            if (dcode > 3) nextra_bits = (int)((dcode-2) / 2);
            int o = bits(data, b, nextra_bits);
            int distance = min_distance + o;
            unsigned long long start = byteIndex - distance;
            for (unsigned long long i = 0; i < length;i++) {
                (*output)[byteIndex++] = (*output)[start+i];
                if (byteIndex >= totalLength) {
                    totalLength += 256;
                    (*output) = (byte*) realloc(*output, totalLength * sizeof(byte));
                }
            }

        }
    }

    return byteIndex;

}


unsigned int decompress(byte* data, byte** output) {
    unsigned long long b = 0;

    int CM = bits(data, &b, 4);
    int CINFO = bits(data, &b, 4);
    int FLGS = bits(data, &b, 8);

    int byteIndex = 0;
    int last = 0;
    int i = 0;
    (*output) = (byte*) malloc(256 * sizeof(byte));
    do {
        i++;
        byteIndex = decompress_chunk(data, output, &b, byteIndex, &last);
    } while (!last);
    return byteIndex;

    }


int png_open(char* file_name, unsigned char** output, int* width, int* height, int* nrchannels) {
    FILE *file = fopen(file_name, "rb");
    if (!file) {
        printf("INVALID IMAGE\n");
        return 0;
    }

    byte png_signature[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
    for (int c = 0; c < 8; c++) {
        if (png_signature[c] != fgetc(file)) {

            printf("ERROR: This isn't a PNG File");
            return -1;
        }
    }
    
    chunk IHDR = read_chunk(file);
    if (strcmp(IHDR.name, "IHDR") != 0) {
        printf("ERROR: First chunk not Header!\n");
        return -1;
    }
    const unsigned int IMAGE_WIDTH = (IHDR.data[0] << 24) + 
                            (IHDR.data[1] << 16) +
                            (IHDR.data[2] << 8) +
                            (IHDR.data[3]);
    const unsigned int IMAGE_HEIGHT = (IHDR.data[4] << 24) + 
                            (IHDR.data[5] << 16) +
                            (IHDR.data[6] << 8) +
                            (IHDR.data[7]);
    const byte BIT_DEPTH = IHDR.data[8]; 
    const byte COLOR_TYPE = IHDR.data[9]; 
    const byte INTERLACE_METHOD = IHDR.data[12]; 
    
    byte CHANNEL_COUNT = 0;
    if (COLOR_TYPE == 0) CHANNEL_COUNT = 1;
    else if (COLOR_TYPE == 2) CHANNEL_COUNT = 3;
    else if (COLOR_TYPE == 3) CHANNEL_COUNT = 3;
    else if (COLOR_TYPE == 4) CHANNEL_COUNT = 2;
    else if (COLOR_TYPE == 6) CHANNEL_COUNT = 4;

    

    byte* palette_data;
    byte palette_len = 0;

    byte* idat_data = (byte*) malloc(256 * sizeof(byte));
    unsigned long long idat_len = 0;
    int idat_block_amount = 0;

    while (1) {
        chunk c_chunk = read_chunk(file);
        if (strcmp(c_chunk.name, "PLTE") == 0 && COLOR_TYPE == 3) {
            palette_len = (byte)(c_chunk.length / 3); 
            if (ceil(c_chunk.length / 3) != palette_len) return -1;
            
            palette_data = (byte*) malloc(c_chunk.length * sizeof(byte));
            for (int i = 0; i < c_chunk.length; i++) {
                palette_data[i] = c_chunk.data[i];
            }
        } else if (strcmp(c_chunk.name, "IDAT") == 0) {
            idat_block_amount++;
            idat_data = (byte*) realloc(idat_data,(idat_len + c_chunk.length) * sizeof(byte));
            for (int i = idat_len; i < idat_len+c_chunk.length;i++) {
                idat_data[i] = c_chunk.data[i-idat_len];
            }
            idat_len += c_chunk.length;

        } else if (strcmp(c_chunk.name, "IEND") == 0) {
            break;
        }
    }

    byte *data;
    unsigned long long len = 0;
    len = decompress(idat_data, &data);
    free(idat_data);
    // give extra waste space at end
    int cc = CHANNEL_COUNT;
    if (palette_len != 0) cc = 1;
    data = (byte*) realloc(data, (len+cc) * sizeof(byte));
    for (int q = 0; q < cc; q++) data[len+q] = 0;
    int linesize = IMAGE_WIDTH*cc+1;
    for (int i = 0; i < IMAGE_HEIGHT; i++) {
        int filter_type = data[linesize * i];
        for (int j = 0; j < IMAGE_WIDTH; j++) {
            int t;
            if (filter_type == 1) {
                byte *x = data + linesize*i + j*cc + 1;
                byte *a = data + linesize*i + (j-1)*cc + 1;
                if (j == 0) a = data + len;
                for (t=0;t<cc;t++) *(x+t) += *(a+t);
            } else if (filter_type == 2) {
                byte *x = data + linesize*i + j*cc + 1;
                byte *b = data + linesize*(i-1) + j*cc + 1;
                if (i == 0) b = data + len;
                for (t=0;t<cc;t++) *(x+t) += *(b+t);
            } else if (filter_type == 3) {
                byte *x = data + linesize*i + j*cc + 1;
                byte *a = data + linesize*i + (j-1)*cc + 1;
                byte *b = data + linesize*(i-1) + j*cc + 1;
                if (j == 0) a = data + len;
                if (i == 0) b = data + len;
                for (t=0;t<cc;t++) *(x+t) += floor((*(b+t) + *(a+t)) / 2.0);
            } else if (filter_type == 4) {
                byte *x = data + linesize*i + j*cc + 1;
                byte *a = data + linesize*i + (j-1)*cc + 1;
                byte *b = data + linesize*(i-1) + j*cc + 1;
                byte *c = data + linesize*(i-1) + (j-1)*cc + 1;
                if (j == 0) a = data + len;
                if (i == 0) b = data + len;
                if (i == 0 || j == 0) c = data + len;
                for (t=0;t<cc;t++){
                    int p = *(a+t) + *(b+t) - *(c+t);
                    int pa = abs(p - *(a+t));
                    int pb = abs(p - *(b+t));
                    int pc = abs(p - *(c+t));
                    if (pa <= pb && pa <= pc) *(x+t) += *(a+t);
                    else if (pb <= pc) *(x+t) += *(b+t);
                    else *(x+t) += *(c+t);
                }

            }
        }
    }

    // Get rid of filter bytes
    for (int i = 0; i < len; i++) {
        if (i % linesize == 0) continue;
        data[i - ((int)(i/linesize) + 1)] = data[i];
    }
    len -= IMAGE_HEIGHT;

    if (palette_len != 0) { // PALETTE
        byte* act_data = (byte*) malloc(len * 3 * sizeof(byte));
        for (int i = 0; i < len; i++) {
            act_data[i*3] = palette_data[data[i]*3];
            act_data[i*3+1] = palette_data[data[i]*3+1];
            act_data[i*3+2] = palette_data[data[i]*3+2];
        }
        len *= 3;
        data = (byte*) realloc(data, len * sizeof(byte));
        for (int i = 0; i < len; i++) {
            data[i] = act_data[i];
        }
        free(act_data);
        free(palette_data);
    } else {
        data = (byte*) realloc(data, len * sizeof(byte));
    }



    (*output) = data;

    *width = IMAGE_WIDTH;
    *height = IMAGE_HEIGHT;
    *nrchannels = CHANNEL_COUNT;

    fclose(file);
    return 0;
}


/* --------------- ACTUAL APPLICATION -------------------------- */

const char* squareVSS = "#version 330 core\n" \
                           "layout (location = 0) in vec3 aPos;\n" \
                           "uniform vec2 pos;\n" \
                           "void main(){\n" \
                           "\tgl_Position = vec4(aPos.x + pos.x, aPos.y + pos.y, aPos.z, 1.0);\n" \
                           "}\n";


const char* textureVSS = "#version 330 core\n" \
                           "layout (location = 0) in vec3 aPos;\n" \
                           "layout (location = 1) in vec2 aTexCoord;\n" \
                           "uniform vec2 pos;\n" \
                           "out vec2 texCoord;\n" \
                           "void main(){\n" \
                           "\tgl_Position = vec4(aPos.x + pos.x, aPos.y + pos.y, aPos.z, 1.0);\n" \
                           "\ttexCoord = aTexCoord;\n" \
                           "}\n";

const char* textureFSS = "#version 330 core\n" \
                            "out vec4 FragColor;\n" \
                            "in vec2 texCoord;\n" \
                            "uniform sampler2D inTexture;\n" \
                            "void main() {\n" \
                            "\tFragColor = texture(inTexture, vec2(texCoord.x, texCoord.y));\n" \
                            "}\n";

const char* colorFSS = "#version 330 core\n" \
                            "out vec4 FragColor;\n" \
                            "uniform vec4 color;\n" \
                            "void main() {\n" \
                            "\tFragColor = color;\n" \
                            "}\n";


typedef GLFWwindow* SGFwindow;
typedef unsigned int SGFshader;
typedef unsigned int SGFtexture;
typedef struct {
    SGFshader shader;
    unsigned int vao;
} SGFobject;

typedef struct {
    SGFobject object;
    SGFtexture texture;
} SGFsprite;


void SGFFramebufferSizeCallback(GLFWwindow* window, int width, int height) {
    glViewport(0, 0, width, height);
}


SGFwindow SGFCreateWindow(int width, int height, char* title, int fullscreen) {
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    
    SGFwindow window;
    if (fullscreen) window = glfwCreateWindow(width,height,title, glfwGetPrimaryMonitor(), NULL);
    else window = glfwCreateWindow(width, height, title, NULL, NULL);
    if (window == NULL) {
        printf("SGF: Failed at creating window\n");
        glfwTerminate();
        return NULL;
    }

    glfwMakeContextCurrent(window);
    glfwSetFramebufferSizeCallback(window, SGFFramebufferSizeCallback);

    return window;
}


SGFshader SGFCreateShader(const char* vertexShader, const char* fragmentShader) {
    unsigned int vs;
    vs = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vs, 1, &vertexShader, NULL);
    glCompileShader(vs);
    int success;
    char error_log[512];
    glGetShaderiv(vs, GL_COMPILE_STATUS, &success);
    if (!success) {
        glGetShaderInfoLog(vs, 512, NULL, error_log);
        printf("ERROR IN VERTEX SHADER:\n%s\n", error_log);
    }


    unsigned int fs;
    fs = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fs, 1, &fragmentShader, NULL);
    glCompileShader(fs);
    glGetShaderiv(fs, GL_COMPILE_STATUS, &success);
    if (!success) {
        glGetShaderInfoLog(fs, 512, NULL, error_log);
        printf("ERROR IN FRAGMENT SHADER:\n%s\n", error_log);
    }

    SGFshader shader;
    shader = glCreateProgram();
    glAttachShader(shader, vs);
    glAttachShader(shader, fs);
    glLinkProgram(shader);
    glGetProgramiv(shader, GL_LINK_STATUS, &success);
    if (!success) {
        glGetProgramInfoLog(shader, 512, NULL, error_log);
        printf("ERROR IN SHADER COMPILE:\n%s\n", error_log);
    }

    glDeleteShader(vs);
    glDeleteShader(fs);
    return shader;
}

unsigned int SGFCreateVAO() {
    unsigned int vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    return vao;
}

unsigned int SGFCreateVBO(void* data, size_t size) {
    unsigned int vbo;
    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, size, data, GL_STATIC_DRAW);
    return vbo;
}

unsigned int SGFCreateEBO(void* data, size_t size) {
    unsigned int ebo;
    glGenBuffers(1, &ebo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, size, data, GL_STATIC_DRAW);
    return ebo;
}

void SGFVAP(int amount, int* sizes, int type, size_t size) {
    int total_size = 0;
    for (int i = 0; i < amount; i++) total_size+=sizes[i];
    int size_til_now = 0;
    for (int i = 0; i < amount; i++){
        glVertexAttribPointer(i, sizes[i], type, GL_FALSE, total_size * size, (void*) (size_til_now * size));
        glEnableVertexAttribArray(i);
        size_til_now += sizes[i];
    }
}

SGFobject SGFCreateSquare(SGFwindow window, int pos_x, int pos_y, int width, int height) {
    int window_width, window_height;
    glfwGetWindowSize(window, &window_width, &window_height);

    float left = (((float)pos_x / (float)window_width) - 0.5f) * 2.0f;
    float right = (((float)(pos_x + width) / (float)window_width) - 0.5f) * 2.0f;
    float bottom = (((float)pos_y / (float)window_height) - 0.5f) * -2.0f;
    float top = (((float)(pos_y + height) / (float)window_height) - 0.5f) * -2.0f;


    float vertices[] = {
        right, top, 0.0f, 1.0f, 1.0f,
        right,bottom, 0.0f, 1.0f, 0.0f,
        left,bottom, 0.0f, 0.0f, 0.0f,
        left, top, 0.0f, 0.0f, 1.0f
    };

    unsigned int indices[] = {
        0, 1, 2,
        0, 2, 3
    };

    unsigned int VAO = SGFCreateVAO();
    unsigned int VBO = SGFCreateVBO(vertices, sizeof(vertices));
    unsigned int EBO = SGFCreateEBO(indices, sizeof(indices));
    int sizes[] = {3,2};
    SGFVAP(2, sizes, GL_FLOAT, sizeof(float));
    
    glBindVertexArray(0);
    
    glBindVertexArray(0);
    SGFobject object;
    object.shader = SGFCreateShader(textureVSS, textureFSS);
    object.vao = VAO;
    return object;
}


SGFtexture SGFLoadTexture(char* texturePath, int isAntiAliased) {
    unsigned int texture;
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);	
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, isAntiAliased ? GL_LINEAR : GL_NEAREST);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    
    int width, height, nrChannels;
    unsigned char* data;
    int error = png_open(texturePath, &data, &width, &height, &nrChannels);
    if (data) {
        if (nrChannels == 4) glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);
        else if (nrChannels == 3) glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
        else printf("Invalid number of channels in Image!\n");
        glGenerateMipmap(GL_TEXTURE_2D);
    } else {
        printf("Image Load failiure\n");
    }

    free(data);

    return texture;
}

SGFsprite SGFCreateSprite(SGFwindow win, int x, int y, int w, int h, char* p, int a) {
    SGFsprite s;
    s.object = SGFCreateSquare(win, x, y, w, h);
    s.texture = SGFLoadTexture(p, a);
    return s;
}


void SGFSetShaderFloatProperty(SGFshader shader, char* property, double value, ...) {
    va_list ptr;
    va_start(ptr, value);
    float values[4];
    for (int i = 0; i < value; i++) values[i] = va_arg(ptr, double);
    va_end(ptr);
    if (value == 1) glUniform1f(glGetUniformLocation(shader, "pos"), values[0]);
    if (value == 2) glUniform2f(glGetUniformLocation(shader, "pos"), values[0], values[1]);
    if (value == 3) glUniform3f(glGetUniformLocation(shader, "pos"), values[0], values[1], values[2]);
    if (value == 4) glUniform4f(glGetUniformLocation(shader, "pos"), values[0], values[1], values[2], values[3]);
}

int SGFRunning(SGFwindow window) {
    return !glfwWindowShouldClose(window);
}


void SGFFillColor(float r, float g, float b, float a) {
    glClearColor(r, g, b, a);
    glClear(GL_COLOR_BUFFER_BIT);
}

void SGFDrawObject(SGFobject obj) {
    glUseProgram(obj.shader);
    glBindVertexArray(obj.vao);
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}


void SGFDrawSprite(SGFsprite s) {
    glBindTexture(GL_TEXTURE_2D, s.texture);
    SGFDrawObject(s.object);
}

void SGFLoopEnd(SGFwindow window) {
    glfwSwapBuffers(window);
    glfwPollEvents();
}

void SGFTerminate() {
    glfwTerminate();
}

#endif
