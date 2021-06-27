let computeVert = 'precision highp float;\n' +
  '\n' +
  'attribute vec2 aPosition;\n' +
  '\n' +
  'varying vec2 vUv;\n' +
  '\n' +
  'void main() {\n' +
  '    gl_Position = vec4(aPosition, 0.0, 1.0);\n' +
  '    vUv = aPosition * 0.5 + 0.5;\n' +
  '}';

let computeFrag = 'precision highp float;\n' +
  '\n' +
  '\n' +
  'uniform sampler2D uPrevBuffer;   // previous ping-pong texture\n' +
  'uniform sampler2D uPebbles;      // 2D pebbles texture\n' +
  'uniform sampler2D uNoiseTexture; // 2D pebbles texture\n' +
  '\n' +
  'uniform float     uTime;\n' +
  'uniform vec2      uResolution;\n' +
  'uniform vec2      uMouse;\n' +
  '\n' +
  'varying vec2 vUv;\n' +
  'const int   VIEW_SAMPLES    = 35;\n' +
  'const int   LIGHT_SAMPLES   = 20;\n' +
  'const int   AMBIENT_SAMPLES = 8;\n' +
  'const float SAMPLING_RATIO = float(VIEW_SAMPLES) / float(LIGHT_SAMPLES);\n' +
  'const float AMBIENT_SAMPLING_RATIO = float(VIEW_SAMPLES) / float(AMBIENT_SAMPLES);\n' +
  '// \t\t\t\t\t    light DIAMETER (not radius!) / samples count\n' +
  'const float STEP_SIZE = (30.0 / float(VIEW_SAMPLES)) * 1.5;\n' +
  'const float STEP_INCREMENTOR = 0.0;\n' +
  'const float LIGHT_STEP_SIZE = STEP_SIZE * SAMPLING_RATIO;\n' +
  '//const float AMBIENT_STEP_SIZE = STEP_SIZE * AMBIENT_SAMPLING_RATIO;\n' +
  'const float DENSITY = 2.8095;\n' +
  '\n' +
  'const float RAIN_CHARGE = 1.0;      \n' +
  'const float LIGHT_STRENGHT = 25.0;\n' +
  'const float AMBIENT_STRENGHT = 0.6;\n' +
  ' \n' +
  'const float REPROJECTION_FACTOR = 3.0;\n' +
  'const float JITTER_STRENGHT = 0.8;\n' +
  '\n' +
  '// #define HALVE_RENDER_SIZE\n' +
  '#define VOLUME_TEXTURES\n' +
  '\n' +
  'const vec3  LIGHT_COLOR = vec3(1.0, 0.4, 0.15) * LIGHT_STRENGHT;\n' +
  '// const float G = 0.4;    // <--- That\'s also VEEEERY NICE!!\n' +
  'const float G = 0.2;\n' +
  'const float G2 = G * G;\n' +
  '\n' +
  'const bool BEERS_POWDER = false;\n' +
  '\n' +
  '\n' +
  '\n' +
  '\n' +
  '\n' +
  '\n' +
  '// Noise generation functions (by iq)\n' +
  'float hash( float n )\n' +
  '{\n' +
  '    return fract(sin(n)*43758.5453);\n' +
  '}\n' +
  '\n' +
  'float fbm( in vec3 x )\n' +
  '{\n' +
  '    // #if 1\n' +
  '    \n' +
  '    vec3 p = floor(x * 2.0);\n' +
  '    vec3 f = fract(x * 2.0);\n' +
  '\tf = f*f*(3.0-2.0*f);\n' +
  '\tvec2 uv = (p.xy+vec2(37.0,17.0)*p.z) + f.xy;\n' +
  '\tvec2 rg = texture2D( uNoiseTexture, (uv+0.5)/256.0, 0.0).yx * 0.6;\n' +
  '\treturn mix( rg.x, rg.y, f.z );\n' +
  '   \n' +
  '}\n' +
  '// #endif\n' +
  '\n' +
  '// float fbm( vec3 p )\n' +
  '// {\n' +
  '//     mat3 m = mat3( 0.00,  0.80,  0.60,\n' +
  '//               -0.80,  0.36, -0.48,\n' +
  '//               -0.60, -0.48,  0.64 );    \n' +
  '//     float f;\n' +
  '//     f  = 0.5000*noise( p ); p = m*p*2.02;\n' +
  '//     f += 0.2500*noise( p ); p = m*p*2.03;\n' +
  '//     f += 0.1250*noise( p );\n' +
  '//     return f;\n' +
  '// }\n' +
  '\n' +
  'float rand(vec2 n) { \n' +
  '\treturn fract(sin(dot(n, vec2(12.9898, 4.1414))) * 43758.5453);\n' +
  '}\n' +
  '\n' +
  'float numericalMieFit(float costh)\n' +
  '{\n' +
  '    // This function was optimized to minimize (delta*delta)/reference in order to capture\n' +
  '    // the low intensity behavior.\n' +
  '    float bestParams[10];\n' +
  '    bestParams[0]=9.805233e-06;\n' +
  '    bestParams[1]=-6.500000e+01;\n' +
  '    bestParams[2]=-5.500000e+01;\n' +
  '    bestParams[3]=8.194068e-01;\n' +
  '    bestParams[4]=1.388198e-01;\n' +
  '    bestParams[5]=-8.370334e+01;\n' +
  '    bestParams[6]=7.810083e+00;\n' +
  '    bestParams[7]=2.054747e-03;\n' +
  '    bestParams[8]=2.600563e-02;\n' +
  '    bestParams[9]=-4.552125e-12;\n' +
  '    \n' +
  '    float p1 = costh + bestParams[3];\n' +
  '    vec4 expValues = exp(vec4(bestParams[1] *costh+bestParams[2], bestParams[5] *p1*p1, bestParams[6] *costh, bestParams[9] *costh));\n' +
  '    vec4 expValWeight= vec4(bestParams[0], bestParams[4], bestParams[7], bestParams[8]);\n' +
  '    return dot(expValues, expValWeight);\n' +
  '}\n' +
  '\n' +
  'float HenyeyGreensteinPhase(float LdV) {\n' +
  '    // return 1.0;\n' +
  '\treturn 0.07957747154 * ((1.0 - G2) / (1.0 + G2 - 2.0 * G * pow(LdV, 3.0 / 2.0)));\n' +
  '}\n' +
  '\n' +
  'float sphereDensity(vec3 point) {\n' +
  '    float distance = length(point - vec3(0.0, 0.0, 20.0));\n' +
  '    if (distance > 15.0) return 0.0;\n' +
  '    \n' +
  '    \n' +
  '    float distance_attenuator = pow(1.0 - distance / 15.0, 1.3);\n' +
  '    /*float distance_attenuator = 1.0;\n' +
  '    if(point.z < 7.0 || point.z > 15.0)   distance_attenuator = 0.0;    \n' +
  '    //if(point.x < -10.0 || point.x > 10.0) distance_attenuator = 0.0;\n' +
  '    if(point.y < -6.0 || point.y > 6.0)   distance_attenuator = 0.0;*/\n' +
  '   \n' +
  '    float increaser = 1.0 - (distance / 6.0);\n' +
  '    increaser = clamp(increaser, 0.0, 1.0);\n' +
  '    increaser = pow(increaser, 0.75);\n' +
  '    \n' +
  '    \n' +
  '    float largeWeather = distance_attenuator * 5.0;\n' +
  '    float den = max(0.0, largeWeather - 0.9*fbm(point * 0.4 + uTime * 0.2));\n' +
  '    if(den <= 0.0)\n' +
  '        return 0.0;\n' +
  '    \n' +
  '    \n' +
  '    den= max(0.0, den-0.2*fbm(point*0.05));\n' +
  '    \n' +
  '    if(point.y > 2.0) {\n' +
  '    \tfloat decreaser = 1.0 - texture2D(uPebbles /*iChannel1*/, point.xz * 0.025 +uTime * 0.02).x;\n' +
  '        decreaser = pow(decreaser, 6.0) * (point.y - 2.0);\n' +
  '        //largeWeather -= decreaser;\n' +
  '    }\n' +
  '    \n' +
  '    if(largeWeather < 0.0) largeWeather = 0.0;\n' +
  '    \n' +
  '    \n' +
  '    largeWeather = largeWeather*0.06*min(1.0, 5.0*den);\n' +
  '    \n' +
  '    if(point.y > 0.5) {\n' +
  '\t\tfloat buggerStrenght = -0.03 + (point.y - 0.5) * 0.0075 + abs(point.x) * 0.01;\n' +
  '    \n' +
  '        if(buggerStrenght < 0.0) buggerStrenght = 0.0;\n' +
  '        \n' +
  '        // float bugger = pow(texture2D(uPebbles, point.xy * 0.05 + vec2(uTime * 0.02, 0.0)).x, 2.0) * 1.5;\n' +
  '        float bugger = pow(texture2D(uNoiseTexture, point.xy * 0.005 + vec2(uTime * 0.002, 0.0)).x, 3.0) * 1.5;\n' +
  '        bugger *= buggerStrenght;\n' +
  '        float buggedLargeWeather = clamp(largeWeather, 0.0, bugger);\n' +
  '        float t = clamp((point.y - 0.5) * 1.0, 0.0, 1.0);\n' +
  '        \n' +
  '        largeWeather = buggedLargeWeather * t + largeWeather * (1.0 - t); // smoothstep(buggedLargeWeather, largeWeather, 0.07);\n' +
  '    \n' +
  '        if(point.y > 1.5 && point.z > 15.0) {\n' +
  '        \t// largeWeather *= 0.1;\n' +
  '        \tfloat t2 = length(point.xy);\n' +
  '        \tt2 = clamp(   pow(max(t2 - 7.0, 0.0), 3.5)    , 0.0, 1000.0) * 0.000524;\n' +
  '        \n' +
  '    \t\tlargeWeather = clamp(largeWeather * 2.0, 0.0, t2);\t\n' +
  '        }\n' +
  '    }\n' +
  '    if(point.y < 1.0) {\n' +
  '    \t//largeWeather = largeWeather * (1.0 + abs(1.0 - point.y) * 0.3);\n' +
  '    }\n' +
  '    if(point.y < -3.0) {\n' +
  '        //largeWeather = clamp(        largeWeather * (1.0 - abs(-3.0 - point.y) * 0.25),      0.0, 1.0);\n' +
  '    }\n' +
  '    \n' +
  '    \n' +
  '    //if (point.z > 6.0 && point.z < 8.5)\n' +
  '    //\tlargeWeather += 0.4*fbm(point * 2.115 + uTime * 0.2);\n' +
  '            \n' +
  '    \n' +
  '    \n' +
  '    return largeWeather + increaser * 1.0;\n' +
  '    \n' +
  '    \n' +
  '    \n' +
  '    float computedDensity = largeWeather;\n' +
  '    computedDensity = clamp(computedDensity, 0.0, 6.0);\n' +
  '    \n' +
  '    \n' +
  '\treturn computedDensity;\n' +
  '}\n' +
  '\n' +
  '\n' +
  'float calculateDensity(vec3 raymarchPosition) {\n' +
  '\treturn sphereDensity(raymarchPosition);\n' +
  '}\n' +
  '\n' +
  '// float calculateTransmittance(float density) {\n' +
  '// \t return clamp(2.0 * exp(-density) * (1.0 - exp(-density * 2.0)), 0.0, 1.0);\n' +
  '// }\n' +
  '\n' +
  '\n' +
  'vec3 sampleSun(vec3 samplePosition, vec3 lightDir) {\n' +
  '    \n' +
  '    float accumulatedLinearDensity = 0.0;\n' +
  '    float transmittance = 1.0;\n' +
  '    \n' +
  '    vec3 raymarchPosition = samplePosition + lightDir * STEP_SIZE * JITTER_STRENGHT * hash(dot(samplePosition * 5.0, vec3(12.256, 2.646, 6.356)) + uTime);\n' +
  '    for(int i = 0; i < LIGHT_SAMPLES; i++) {\n' +
  '    \t\n' +
  '    \traymarchPosition += lightDir * LIGHT_STEP_SIZE;\n' +
  '        \n' +
  '        float densityAtPoint = calculateDensity(raymarchPosition) * DENSITY; //LIGHT_DENSITY;\n' +
  '        \n' +
  '        accumulatedLinearDensity += densityAtPoint;\n' +
  '        \n' +
  '        transmittance *= exp( -densityAtPoint * LIGHT_STEP_SIZE );\n' +
  '        //transmittance = calculateTransmittance( accumulatedLinearDensity * LIGHT_STEP_SIZE );\n' +
  '    }\n' +
  '    \n' +
  '    \n' +
  '    if(BEERS_POWDER) \n' +
  '        return LIGHT_COLOR * transmittance * (1.0 - exp(-accumulatedLinearDensity * LIGHT_STEP_SIZE * float(LIGHT_SAMPLES)));\n' +
  '    else\n' +
  '    \treturn LIGHT_COLOR * transmittance;\n' +
  '}\n' +
  '\n' +
  '\n' +
  'float Ei( float z )\n' +
  '{\n' +
  '    \n' +
  '\treturn 0.5772156649015328606065 + log( 0.0001 + abs(z) ) + z * (1.0 + z * (0.25 + z * ( (1.0/18.0) + z * ( (1.0/96.0) + z *\n' +
  '\t\t   (1.0/600.0) ) ) ) ); // For x!=0\n' +
  '    \n' +
  '\treturn 0.5772156649015328606065 + log( 1e-4 + abs(z) ) + z * (1.0 + z * (0.25 + z * ( (1.0/18.0) + z * ( (1.0/96.0) + z *\n' +
  '\t\t   (1.0/600.0) ) ) ) ); // For x!=0\n' +
  '}\n' +
  '\n' +
  'vec3 sampleAmbient(vec3 samplePosition, float density) {\n' +
  '     \n' +
  '    \n' +
  '    float VolumeTop = 10.0;\n' +
  '    float VolumeBottom = -10.0;\n' +
  '    vec3 IsotropicLightTop = vec3(0.25, 0.66, 0.9) * AMBIENT_STRENGHT;\n' +
  '    vec3 IsotropicLightBottom = vec3(0.75, 0.3, 0.2) * AMBIENT_STRENGHT;\n' +
  '    \n' +
  '    //float Hp = VolumeTop - samplePosition.y; // Height to the top of the volume\n' +
  '    \n' +
  '    float AMBIENT_STEP_SIZE = (VolumeTop - VolumeBottom) / float(AMBIENT_SAMPLES * 3);\n' +
  '    \n' +
  '    \n' +
  '    {\n' +
  '    float accumulatedLinearDensity = 0.0;\n' +
  '    \n' +
  '    vec3 raymarchPosition = samplePosition + vec3(0.0, 1.0, 0.0) * STEP_SIZE * JITTER_STRENGHT * hash(dot(samplePosition * 5.0, vec3(12.256, 2.646, 6.356)) + uTime);;\n' +
  '    for(int i = 0; i < AMBIENT_SAMPLES; i++) {\n' +
  '    \t\n' +
  '    \traymarchPosition += vec3(0.0, 1.0, 0.0) * AMBIENT_STEP_SIZE;\n' +
  '        \n' +
  '        float densityAtPoint = calculateDensity(raymarchPosition) * DENSITY; //LIGHT_DENSITY;\n' +
  '        accumulatedLinearDensity += densityAtPoint;\n' +
  '    }\n' +
  '    density = accumulatedLinearDensity;\n' +
  '    }\n' +
  '    \n' +
  '    \n' +
  '    \n' +
  '    float a = -density * AMBIENT_STEP_SIZE * float(AMBIENT_SAMPLES) * 0.06;// * Hp;\n' +
  '    vec3 IsotropicScatteringTop = IsotropicLightTop * max( 0.0, exp( a ) - a * Ei( a ));\n' +
  '    //float Hb = samplePosition.y - VolumeBottom; // Height to the bottom of the volume\n' +
  '    \n' +
  '\n' +
  '    \n' +
  '    \n' +
  '    {\n' +
  '    float accumulatedLinearDensity = 0.0;\n' +
  '    \n' +
  '    vec3 raymarchPosition = samplePosition;\n' +
  '    for(int i = 0; i < AMBIENT_SAMPLES; i++) {\n' +
  '    \t\n' +
  '    \traymarchPosition += vec3(0.0, -1.0, 0.0) * AMBIENT_STEP_SIZE;\n' +
  '        \n' +
  '        float densityAtPoint = calculateDensity(raymarchPosition) * DENSITY; //LIGHT_DENSITY;\n' +
  '        \n' +
  '        accumulatedLinearDensity += densityAtPoint;\n' +
  '    }\n' +
  '    density = accumulatedLinearDensity;\n' +
  '    }\n' +
  '    \n' +
  '    \n' +
  '    a = -density * AMBIENT_STEP_SIZE * float(AMBIENT_SAMPLES) * 0.06;// * Hb;\n' +
  '    vec3 IsotropicScatteringBottom = IsotropicLightBottom * max( 0.0, exp( a ) - a * Ei( a ));\n' +
  '\n' +
  '    \n' +
  '    //return vec3(0.0);\n' +
  '    return (IsotropicScatteringTop + IsotropicScatteringBottom) * AMBIENT_STRENGHT;\n' +
  '    \n' +
  '    /*float accumulatedLinearDensity = 0.0;\n' +
  '    float transmittance = 1.0;\n' +
  '    \n' +
  '    vec3 rayDir = -lightDir; // vec3(0.0, 1.0, 0.0);\n' +
  '    if (pass > 0.0) rayDir = vec3(0.0, -1.0, 0.0);\n' +
  '    \n' +
  '    vec3 ambientColor = vec3(0.25, 0.66, 0.9);\n' +
  '    if (pass > 0.0) ambientColor = vec3(0.75, 0.3, 0.2);\n' +
  '\n' +
  '    ambientColor *= AMBIENT_STRENGHT;\n' +
  '    \n' +
  '    vec3 raymarchPosition = samplePosition;\n' +
  '    for(int i = 0; i < LIGHT_SAMPLES; i++) {\n' +
  '    \t\n' +
  '    \traymarchPosition += rayDir * LIGHT_STEP_SIZE;\n' +
  '        \n' +
  '        float densityAtPoint = calculateDensity(raymarchPosition) * LIGHT_DENSITY;\n' +
  '        \n' +
  '        accumulatedLinearDensity += densityAtPoint;   \n' +
  '        \n' +
  '        transmittance *= exp( -densityAtPoint * LIGHT_STEP_SIZE );\n' +
  '    }\n' +
  '    \n' +
  '    return ambientColor * transmittance;*/\n' +
  '}\n' +
  '\n' +
  '\n' +
  'vec3 robobo1221Tonemap(vec3 color)\n' +
  '{\n' +
  '    #define rTOperator(x) (x / sqrt(x*x+1.0))\n' +
  '\n' +
  '    float l = length(color);\n' +
  '\n' +
  '    color = mix(color, color * 0.5, l / (l+1.0));\n' +
  '    color = rTOperator(color);\n' +
  '\n' +
  '    return color;\n' +
  '}\n' +
  '\n' +
  'void main()\n' +
  '{\n' +
  '    // Normalized pixel coordinates (from 0 to 1)\n' +
  '    vec2 uv = gl_FragCoord.xy/uResolution.xy;\n' +
  '\tfloat aspect = uResolution.x / uResolution.y;\n' +
  ' \n' +
  '   \n' +
  '    vec4 prevColor = vec4(0.0); // texture(iChannel0, uv);\n' +
  '    \n' +
  '    \n' +
  '    #ifdef HALVE_RENDER_SIZE\n' +
  '    // at the moment this doesn\'t seem to help with performance\n' +
  '      float pixnum = mod(gl_FragCoord.x, 2.0) + mod(gl_FragCoord.y, 2.0);\n' +
  '      float pixtarget = mod(float(iFrame), 4.0);\n' +
  '    \n' +
  '      if(pixnum != pixtarget) {\n' +
  '          fragColor = prevColor;\n' +
  '       \t  return;\n' +
  '      }\n' +
  '    #endif\n' +
  '    \n' +
  '    \n' +
  '    vec2 ndc = uv * 2.0 - 1.0;\n' +
  '    ndc.x *= aspect;\n' +
  '    \n' +
  '    vec3 rayOrigin = vec3(0.0);\n' +
  '    vec3 rayDir    = normalize(     vec3(0.0, 0.0, 1.0) + vec3(ndc.xy, 0.0)     );\n' +
  '    \n' +
  '    vec2 mousePosNDC = (uMouse.xy / uResolution.xy) * 2.0 - 1.0;\n' +
  '    mousePosNDC.x *= aspect;\n' +
  '    vec3 lightDir  = normalize(vec3(mousePosNDC.xy * 2.0, 1.0));\n' +
  '    \n' +
  '    \n' +
  '    \n' +
  '    \n' +
  '    \n' +
  '    float accumulatedLinearDensity = 0.0;\n' +
  '    \n' +
  '    float LdV         = dot(rayDir, lightDir);\n' +
  '    if(LdV < 0.0) LdV = dot(rayDir, -lightDir);\n' +
  '    float A1dV         = dot(rayDir, vec3(0.0, 1.0, 0.0));\n' +
  '    if(A1dV < 0.0) A1dV = dot(rayDir, -vec3(0.0, 1.0, 0.0));\n' +
  '    float A2dV         = dot(rayDir, vec3(0.0, -1.0, 0.0));\n' +
  '    if(A2dV < 0.0) A2dV = dot(rayDir, -vec3(0.0, -1.0, 0.0));\n' +
  '    \n' +
  '    // LdV = pow(LdV, 0.4);\n' +
  '    \n' +
  '    float PhaseSun      = numericalMieFit(LdV);    \n' +
  '    float PhaseAmbient1 = numericalMieFit(A1dV);\n' +
  '    float PhaseAmbient2 = numericalMieFit(A2dV);\n' +
  '   \n' +
  '    \n' +
  '    vec3 scattering = vec3(0.0);\n' +
  '    vec3 raymarchPosition = rayOrigin + rayDir * 5.0 + rayDir * STEP_SIZE * JITTER_STRENGHT * hash(dot(rayDir * 5.0, vec3(12.256, 2.646, 6.356)) + uTime);\n' +
  '    \n' +
  '\tfloat transmittance = 1.0;\n' +
  '    for(int i = 0; i < VIEW_SAMPLES; i++) {\n' +
  '    \t\n' +
  '    \traymarchPosition += rayDir * STEP_SIZE;\n' +
  '        \n' +
  '        vec3 littleJitter = vec3(0.0, 0.0, rand(gl_FragCoord.xy * 0.34) * 0.4); \n' +
  '        float densityAtPoint = calculateDensity(raymarchPosition/* + littleJitter*/) * DENSITY;\n' +
  '        \n' +
  '        // skip non-contributing sample\n' +
  '        if(densityAtPoint == 0.0) continue;\n' +
  '   \n' +
  '        \n' +
  '        // from now on a light sample is required\n' +
  '        accumulatedLinearDensity += densityAtPoint;        \n' +
  '        \n' +
  '      \n' +
  '        // transmittance *= exp( -densityAtPoint * STEP_SIZE );\n' +
  '        // transmittance = calculateTransmittance( accumulatedLinearDensity * STEP_SIZE );\n' +
  '\n' +
  '        \n' +
  '        //scattering += sampleSun(raymarchPosition, phase, lightDir) * transmittance;    \n' +
  '        vec3 SunColor = sampleSun(raymarchPosition, lightDir);\n' +
  '        vec3 AmbientColor1 = sampleAmbient(raymarchPosition, densityAtPoint);\n' +
  '        // vec3 AmbientColor2 = sampleAmbient(raymarchPosition, 1.0);\n' +
  '        vec3 StepScattering = densityAtPoint * /*STEP_SIZE **/ (PhaseSun * SunColor + \n' +
  '                                                                /*PhaseAmbient1 **/ AmbientColor1);// + \n' +
  '                                                              //PhaseAmbient2 * AmbientColor2);\n' +
  '\n' +
  '        \n' +
  ' \t\t//scattering += transmittance * StepScattering; // Accumulate scattering attenuated by extinction\n' +
  '        scattering += transmittance * (StepScattering - StepScattering * exp(-densityAtPoint * STEP_SIZE)) / densityAtPoint;\n' +
  '        \n' +
  '        transmittance *= exp( -densityAtPoint * STEP_SIZE );\n' +
  '    }\n' +
  '    \n' +
  '   \n' +
  '    vec4 computedColor = vec4(scattering, transmittance);\n' +
  '\n' +
  '\t// vec3 backgroundColor = vec3(0.25, 0.66, 0.9) * 0.199; //     * 0.0;\n' +
  '\tvec3 backgroundColor = vec3(0.025, 0.025, 0.025); //     * 0.0;\n' +
  '    vec3 col = mix(vec3(backgroundColor), vec3(computedColor), 1.0 - computedColor.a);\n' +
  '    \n' +
  '    vec3 color = pow(col, vec3(1.0 / 2.2));\n' +
  '    color = robobo1221Tonemap(color);\n' +
  '    \n' +
  '    \n' +
  '    \n' +
  '    // Output to screen\n' +
  '    float rf = 1.0 / REPROJECTION_FACTOR;\n' +
  '    // gl_FragColor = vec4(color * rf + prevColor.rgb * (1.0 - rf), 1.0);\n' +
  '    gl_FragColor = vec4(color, 1.0 / REPROJECTION_FACTOR);\n' +
  '}';