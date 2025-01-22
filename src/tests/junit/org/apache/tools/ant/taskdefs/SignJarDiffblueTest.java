package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class SignJarDiffblueTest {
  /**
   * Test {@link SignJar#add(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#add(FileNameMapper)}
   */
  @Test
  public void testAdd_givenSignJarAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> signJar.add(new CutDirsMapper()));
  }

  /**
   * Test {@link SignJar#add(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>Then {@link SignJar} (default constructor) Mapper is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#add(FileNameMapper)}
   */
  @Test
  public void testAdd_givenSignJar_thenSignJarMapperIsCutDirsMapper() {
    // Arrange
    SignJar signJar = new SignJar();
    CutDirsMapper newMapper = new CutDirsMapper();

    // Act
    signJar.add(newMapper);

    // Assert
    assertSame(newMapper, signJar.getMapper());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SignJar#setDestDir(File)}
   *   <li>{@link SignJar#setDigestAlg(String)}
   *   <li>{@link SignJar#setForce(boolean)}
   *   <li>{@link SignJar#setInternalsf(boolean)}
   *   <li>{@link SignJar#setLazy(boolean)}
   *   <li>{@link SignJar#setPreserveLastModified(boolean)}
   *   <li>{@link SignJar#setSectionsonly(boolean)}
   *   <li>{@link SignJar#setSigAlg(String)}
   *   <li>{@link SignJar#setSigfile(String)}
   *   <li>{@link SignJar#setSignedjar(File)}
   *   <li>{@link SignJar#setTSADigestAlg(String)}
   *   <li>{@link SignJar#setTsacert(String)}
   *   <li>{@link SignJar#setTsaproxyhost(String)}
   *   <li>{@link SignJar#setTsaproxyport(String)}
   *   <li>{@link SignJar#setTsaurl(String)}
   *   <li>{@link SignJar#getDigestAlg()}
   *   <li>{@link SignJar#getMapper()}
   *   <li>{@link SignJar#getSigAlg()}
   *   <li>{@link SignJar#getTSADigestAlg()}
   *   <li>{@link SignJar#getTsacert()}
   *   <li>{@link SignJar#getTsaproxyhost()}
   *   <li>{@link SignJar#getTsaproxyport()}
   *   <li>{@link SignJar#getTsaurl()}
   *   <li>{@link SignJar#isForce()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setDestDir(Copy.NULL_FILE_PLACEHOLDER);
    signJar.setDigestAlg("Digest Alg");
    signJar.setForce(true);
    signJar.setInternalsf(true);
    signJar.setLazy(true);
    signJar.setPreserveLastModified(true);
    signJar.setSectionsonly(true);
    signJar.setSigAlg("Sig Alg");
    signJar.setSigfile("Sigfile");
    signJar.setSignedjar(Copy.NULL_FILE_PLACEHOLDER);
    signJar.setTSADigestAlg("Digest Alg");
    signJar.setTsacert("Tsacert");
    signJar.setTsaproxyhost("localhost");
    signJar.setTsaproxyport("Tsaproxyport");
    signJar.setTsaurl("https://example.org/example");
    String actualDigestAlg = signJar.getDigestAlg();
    FileNameMapper actualMapper = signJar.getMapper();
    String actualSigAlg = signJar.getSigAlg();
    String actualTSADigestAlg = signJar.getTSADigestAlg();
    String actualTsacert = signJar.getTsacert();
    String actualTsaproxyhost = signJar.getTsaproxyhost();
    String actualTsaproxyport = signJar.getTsaproxyport();
    String actualTsaurl = signJar.getTsaurl();

    // Assert
    assertEquals("Digest Alg", actualDigestAlg);
    assertEquals("Digest Alg", actualTSADigestAlg);
    assertEquals("Sig Alg", actualSigAlg);
    assertEquals("Tsacert", actualTsacert);
    assertEquals("Tsaproxyport", actualTsaproxyport);
    assertEquals("https://example.org/example", actualTsaurl);
    assertEquals("localhost", actualTsaproxyhost);
    assertNull(actualMapper);
    assertTrue(signJar.isForce());
  }

  /**
   * Test {@link SignJar#execute()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#execute()}
   */
  @Test
  public void testExecute_givenSignJar() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SignJar()).execute());
  }

  /**
   * Test {@link SignJar#execute()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#execute()}
   */
  @Test
  public void testExecute_givenSignJarAddCutDirsMapper() throws BuildException {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> signJar.execute());
  }

  /**
   * Test {@link SignJar#execute()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#execute()}
   */
  @Test
  public void testExecute_givenSignJarAddFilesetFileSet() throws BuildException {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.addFileset(new FileSet());
    signJar.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> signJar.execute());
  }

  /**
   * Test {@link SignJar#execute()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Alias is {@link SignJar#ERROR_NO_ALIAS}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#execute()}
   */
  @Test
  public void testExecute_givenSignJarAliasIsError_no_alias() throws BuildException {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setAlias(SignJar.ERROR_NO_ALIAS);
    signJar.addFileset(new FileSet());
    signJar.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> signJar.execute());
  }

  /**
   * Test {@link SignJar#execute()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) DestDir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#execute()}
   */
  @Test
  public void testExecute_givenSignJarDestDirIsNull_file_placeholder() throws BuildException {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setDestDir(Copy.NULL_FILE_PLACEHOLDER);
    signJar.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> signJar.execute());
  }

  /**
   * Test {@link SignJar#execute()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Jar is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#execute()}
   */
  @Test
  public void testExecute_givenSignJarJarIsNull_file_placeholder() throws BuildException {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setJar(Copy.NULL_FILE_PLACEHOLDER);
    signJar.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> signJar.execute());
  }

  /**
   * Test {@link SignJar#execute()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Signedjar is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#execute()}
   */
  @Test
  public void testExecute_givenSignJarSignedjarIsNull_file_placeholder() throws BuildException {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setSignedjar(Copy.NULL_FILE_PLACEHOLDER);
    signJar.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> signJar.execute());
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenJavaLangObject_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("UTF8", typeClass);
    project.addBuildListener(new AntClassLoader());

    SignJar signJar = new SignJar();
    signJar.setProject(project);
    signJar.setLazy(true);
    File jarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signJar.isUpToDate(jarFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    SignJar signJar = new SignJar();
    signJar.setProject(project);
    signJar.setLazy(true);
    File jarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signJar.isUpToDate(jarFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) KeepGoingMode is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenProjectKeepGoingModeIsTrue() {
    // Arrange
    Project project = new Project();
    project.setKeepGoingMode(true);
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("UTF8", typeClass);
    project.addBuildListener(new AntClassLoader());

    SignJar signJar = new SignJar();
    signJar.setProject(project);
    signJar.setLazy(true);
    File jarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signJar.isUpToDate(jarFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Force is {@code true}.</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJarForceIsTrue_whenNull_file_placeholder_thenReturnFalse() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setForce(true);

    // Act and Assert
    assertFalse(signJar.isUpToDate(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Lazy is {@code true}.</li>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJarLazyIsTrue_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setLazy(true);
    File jarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signJar.isUpToDate(jarFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJarProjectIsProject() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setProject(new Project());
    signJar.setLazy(true);
    File jarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signJar.isUpToDate(jarFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Sigfile is {@code file.encoding}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJarSigfileIsFileEncoding() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setSigfile("file.encoding");
    signJar.setLazy(true);
    File jarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signJar.isUpToDate(jarFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJar_whenNull_file_placeholder_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new SignJar()).isUpToDate(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJar_whenNull_file_placeholder_thenReturnFalse2() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act and Assert
    assertFalse(signJar.isUpToDate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJar_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new SignJar()).isUpToDate(null, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJar_whenNull_thenReturnFalse2() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act and Assert
    assertFalse(signJar.isUpToDate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), null));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>When Property is {@code java.io.tmpdir} is empty string toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJar_whenPropertyIsJavaIoTmpdirIsEmptyStringToFile() {
    // Arrange
    SignJar signJar = new SignJar();
    File jarFile = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertFalse(signJar.isUpToDate(jarFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SignJar#isUpToDate(File, File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate_givenSignJar_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    SignJar signJar = new SignJar();
    File jarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signJar.isUpToDate(jarFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SignJar#isSigned(File)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isSigned(File)}
   */
  @Test
  public void testIsSigned_givenJavaLangObject_whenNull_file_placeholder_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("UTF8", typeClass);
    project.addBuildListener(new AntClassLoader());

    SignJar signJar = new SignJar();
    signJar.setProject(project);

    // Act and Assert
    assertFalse(signJar.isSigned(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link SignJar#isSigned(File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isSigned(File)}
   */
  @Test
  public void testIsSigned_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    SignJar signJar = new SignJar();
    signJar.setProject(project);

    // Act and Assert
    assertFalse(signJar.isSigned(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link SignJar#isSigned(File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) addArg {@link Argument} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isSigned(File)}
   */
  @Test
  public void testIsSigned_givenSignJarAddArgArgument_thenReturnFalse() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.addArg(new Argument());
    signJar.add(new CutDirsMapper());
    signJar.setSigfile("foo");
    signJar.setAlias(null);

    // Act and Assert
    assertFalse(signJar.isSigned(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link SignJar#isSigned(File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isSigned(File)}
   */
  @Test
  public void testIsSigned_givenSignJarProjectIsProject_thenReturnFalse() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setProject(new Project());

    // Act and Assert
    assertFalse(signJar.isSigned(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link SignJar#isSigned(File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Sigfile is {@code foo}.</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isSigned(File)}
   */
  @Test
  public void testIsSigned_givenSignJarSigfileIsFoo_whenNull_file_placeholder_thenReturnFalse() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setSigfile("foo");
    signJar.setAlias(null);

    // Act and Assert
    assertFalse(signJar.isSigned(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link SignJar#isSigned(File)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SignJar#isSigned(File)}
   */
  @Test
  public void testIsSigned_givenSignJar_whenNull_file_placeholder_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new SignJar()).isSigned(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test new {@link SignJar} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SignJar}
   */
  @Test
  public void testNewSignJar() {
    // Arrange and Act
    SignJar actualSignJar = new SignJar();

    // Assert
    assertNull(actualSignJar.jar);
    assertNull(actualSignJar.destDir);
    assertNull(actualSignJar.signedjar);
    assertNull(actualSignJar.getDescription());
    assertNull(actualSignJar.getTaskName());
    assertNull(actualSignJar.getTaskType());
    assertNull(actualSignJar.getDigestAlg());
    assertNull(actualSignJar.getSigAlg());
    assertNull(actualSignJar.getTSADigestAlg());
    assertNull(actualSignJar.getTsacert());
    assertNull(actualSignJar.getTsaproxyhost());
    assertNull(actualSignJar.getTsaproxyport());
    assertNull(actualSignJar.getTsaurl());
    assertNull(actualSignJar.alias);
    assertNull(actualSignJar.keypass);
    assertNull(actualSignJar.keystore);
    assertNull(actualSignJar.maxMemory);
    assertNull(actualSignJar.storepass);
    assertNull(actualSignJar.storetype);
    assertNull(actualSignJar.sigfile);
    assertNull(actualSignJar.getProject());
    assertNull(actualSignJar.getOwningTarget());
    assertNull(actualSignJar.getRedirector());
    assertNull(actualSignJar.getMapper());
    assertFalse(actualSignJar.hasResources());
    assertFalse(actualSignJar.isForce());
    assertFalse(actualSignJar.strict);
    assertFalse(actualSignJar.verbose);
    assertFalse(actualSignJar.internalsf);
    assertFalse(actualSignJar.lazy);
    assertFalse(actualSignJar.sectionsonly);
    assertTrue(actualSignJar.filesets.isEmpty());
  }
}
