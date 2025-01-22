package org.apache.tools.ant.taskdefs.optional.sos;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Commandline;
import org.junit.Test;

public class SOSDiffblueTest {
  /**
   * Test {@link SOS#setNoCache(boolean)}.
   * <p>
   * Method under test: {@link SOS#setNoCache(boolean)}
   */
  @Test
  public void testSetNoCache() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setNoCache(true);

    // Assert
    assertEquals(SOSCmd.FLAG_NO_CACHE, sosCheckin.getNoCache());
  }

  /**
   * Test {@link SOS#setNoCompress(boolean)}.
   * <p>
   * Method under test: {@link SOS#setNoCompress(boolean)}
   */
  @Test
  public void testSetNoCompress() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setNoCompress(true);

    // Assert
    assertEquals(SOSCmd.FLAG_NO_COMPRESSION, sosCheckin.getNoCompress());
  }

  /**
   * Test {@link SOS#setSosCmd(String)}.
   * <ul>
   *   <li>When {@code Dir}.</li>
   *   <li>Then {@link SOSCheckin} (default constructor) SosCommand is {@code Dir/soscmd}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#setSosCmd(String)}
   */
  @Test
  public void testSetSosCmd_whenDir_thenSOSCheckinSosCommandIsDirSoscmd() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setSosCmd("Dir");

    // Assert
    assertEquals("Dir/soscmd", sosCheckin.getSosCommand());
  }

  /**
   * Test {@link SOS#setSosCmd(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link SOSCheckin} (default constructor) SosCommand is {@code /soscmd}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#setSosCmd(String)}
   */
  @Test
  public void testSetSosCmd_whenEmptyString_thenSOSCheckinSosCommandIsSoscmd() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setSosCmd("");

    // Assert
    assertEquals("/soscmd", sosCheckin.getSosCommand());
  }

  /**
   * Test {@link SOS#setSosCmd(String)}.
   * <ul>
   *   <li>When {@link SOSCmd#PROJECT_PREFIX}.</li>
   *   <li>Then {@link SOSCheckin} (default constructor) SosCommand is {@code $/soscmd}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#setSosCmd(String)}
   */
  @Test
  public void testSetSosCmd_whenProject_prefix_thenSOSCheckinSosCommandIsSoscmd() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setSosCmd(SOSCmd.PROJECT_PREFIX);

    // Assert
    assertEquals("$/soscmd", sosCheckin.getSosCommand());
  }

  /**
   * Test {@link SOS#setUsername(String)}.
   * <p>
   * Method under test: {@link SOS#setUsername(String)}
   */
  @Test
  public void testSetUsername() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setUsername("janedoe");

    // Assert
    assertEquals("janedoe", sosCheckin.getUsername());
  }

  /**
   * Test {@link SOS#setPassword(String)}.
   * <p>
   * Method under test: {@link SOS#setPassword(String)}
   */
  @Test
  public void testSetPassword() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setPassword("iloveyou");

    // Assert
    assertEquals("iloveyou", sosCheckin.getPassword());
  }

  /**
   * Test {@link SOS#setProjectPath(String)}.
   * <ul>
   *   <li>When {@link SOSCmd#PROJECT_PREFIX}.</li>
   *   <li>Then {@link SOSCheckin} (default constructor) ProjectPath is {@link SOSCmd#PROJECT_PREFIX}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#setProjectPath(String)}
   */
  @Test
  public void testSetProjectPath_whenProject_prefix_thenSOSCheckinProjectPathIsProject_prefix() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setProjectPath(SOSCmd.PROJECT_PREFIX);

    // Assert
    assertEquals(SOSCmd.PROJECT_PREFIX, sosCheckin.getProjectPath());
  }

  /**
   * Test {@link SOS#setProjectPath(String)}.
   * <ul>
   *   <li>When {@code Projectpath}.</li>
   *   <li>Then {@link SOSCheckin} (default constructor) ProjectPath is {@code $Projectpath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#setProjectPath(String)}
   */
  @Test
  public void testSetProjectPath_whenProjectpath_thenSOSCheckinProjectPathIsProjectpath() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setProjectPath("Projectpath");

    // Assert
    assertEquals("$Projectpath", sosCheckin.getProjectPath());
  }

  /**
   * Test {@link SOS#setVssServerPath(String)}.
   * <p>
   * Method under test: {@link SOS#setVssServerPath(String)}
   */
  @Test
  public void testSetVssServerPath() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setVssServerPath("Vss Server Path");

    // Assert
    assertEquals("Vss Server Path", sosCheckin.getVssServerPath());
  }

  /**
   * Test {@link SOS#setSosHome(String)}.
   * <p>
   * Method under test: {@link SOS#setSosHome(String)}
   */
  @Test
  public void testSetSosHome() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setSosHome("Sos Home");

    // Assert
    assertEquals("Sos Home", sosCheckin.getSosHome());
  }

  /**
   * Test {@link SOS#setSosServerPath(String)}.
   * <p>
   * Method under test: {@link SOS#setSosServerPath(String)}
   */
  @Test
  public void testSetSosServerPath() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setSosServerPath("Sos Server Path");

    // Assert
    assertEquals("Sos Server Path", sosCheckin.getSosServerPath());
  }

  /**
   * Test {@link SOS#setVerbose(boolean)}.
   * <p>
   * Method under test: {@link SOS#setVerbose(boolean)}
   */
  @Test
  public void testSetVerbose() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setVerbose(true);

    // Assert
    assertEquals(SOSCmd.FLAG_VERBOSE, sosCheckin.getVerbose());
  }

  /**
   * Test {@link SOS#setInternalFilename(String)}.
   * <p>
   * Method under test: {@link SOS#setInternalFilename(String)}
   */
  @Test
  public void testSetInternalFilename() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setInternalFilename("File");

    // Assert
    assertEquals("File", sosCheckin.getFilename());
  }

  /**
   * Test {@link SOS#setInternalRecursive(boolean)}.
   * <p>
   * Method under test: {@link SOS#setInternalRecursive(boolean)}
   */
  @Test
  public void testSetInternalRecursive() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setInternalRecursive(true);

    // Assert
    assertEquals(SOSCmd.FLAG_RECURSION, sosCheckin.getRecursive());
  }

  /**
   * Test {@link SOS#setInternalComment(String)}.
   * <p>
   * Method under test: {@link SOS#setInternalComment(String)}
   */
  @Test
  public void testSetInternalComment() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setInternalComment("Text");

    // Assert
    assertEquals("Text", sosCheckin.getComment());
  }

  /**
   * Test {@link SOS#setInternalLabel(String)}.
   * <p>
   * Method under test: {@link SOS#setInternalLabel(String)}
   */
  @Test
  public void testSetInternalLabel() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setInternalLabel("Text");

    // Assert
    assertEquals("Text", sosCheckin.getLabel());
  }

  /**
   * Test {@link SOS#setInternalVersion(String)}.
   * <p>
   * Method under test: {@link SOS#setInternalVersion(String)}
   */
  @Test
  public void testSetInternalVersion() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setInternalVersion("Text");

    // Assert
    assertEquals("Text", sosCheckin.getVersion());
  }

  /**
   * Test {@link SOS#getSosCommand()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) SosCmd is {@code null}.</li>
   *   <li>Then return {@code /soscmd}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getSosCommand()}
   */
  @Test
  public void testGetSosCommand_givenSOSCheckinSosCmdIsNull_thenReturnSoscmd() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setSosCmd(null);

    // Act and Assert
    assertEquals("/soscmd", sosCheckin.getSosCommand());
  }

  /**
   * Test {@link SOS#getSosCommand()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor).</li>
   *   <li>Then return {@link SOSCmd#COMMAND_SOS_EXE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getSosCommand()}
   */
  @Test
  public void testGetSosCommand_givenSOSCheckin_thenReturnCommand_sos_exe() {
    // Arrange, Act and Assert
    assertEquals(SOSCmd.COMMAND_SOS_EXE, (new SOSCheckin()).getSosCommand());
  }

  /**
   * Test {@link SOS#getComment()}.
   * <p>
   * Method under test: {@link SOS#getComment()}
   */
  @Test
  public void testGetComment() {
    // Arrange, Act and Assert
    assertNull((new SOSCheckin()).getComment());
  }

  /**
   * Test {@link SOS#getVersion()}.
   * <p>
   * Method under test: {@link SOS#getVersion()}
   */
  @Test
  public void testGetVersion() {
    // Arrange, Act and Assert
    assertNull((new SOSCheckin()).getVersion());
  }

  /**
   * Test {@link SOS#getLabel()}.
   * <p>
   * Method under test: {@link SOS#getLabel()}
   */
  @Test
  public void testGetLabel() {
    // Arrange, Act and Assert
    assertNull((new SOSCheckin()).getLabel());
  }

  /**
   * Test {@link SOS#getUsername()}.
   * <p>
   * Method under test: {@link SOS#getUsername()}
   */
  @Test
  public void testGetUsername() {
    // Arrange, Act and Assert
    assertNull((new SOSCheckin()).getUsername());
  }

  /**
   * Test {@link SOS#getPassword()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) Password is {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getPassword()}
   */
  @Test
  public void testGetPassword_givenSOSCheckinPasswordIsFoo_thenReturnFoo() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setPassword("foo");

    // Act and Assert
    assertEquals("foo", sosCheckin.getPassword());
  }

  /**
   * Test {@link SOS#getPassword()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getPassword()}
   */
  @Test
  public void testGetPassword_givenSOSCheckin_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new SOSCheckin()).getPassword());
  }

  /**
   * Test {@link SOS#getProjectPath()}.
   * <p>
   * Method under test: {@link SOS#getProjectPath()}
   */
  @Test
  public void testGetProjectPath() {
    // Arrange, Act and Assert
    assertNull((new SOSCheckin()).getProjectPath());
  }

  /**
   * Test {@link SOS#getVssServerPath()}.
   * <p>
   * Method under test: {@link SOS#getVssServerPath()}
   */
  @Test
  public void testGetVssServerPath() {
    // Arrange, Act and Assert
    assertNull((new SOSCheckin()).getVssServerPath());
  }

  /**
   * Test {@link SOS#getSosHome()}.
   * <p>
   * Method under test: {@link SOS#getSosHome()}
   */
  @Test
  public void testGetSosHome() {
    // Arrange, Act and Assert
    assertNull((new SOSCheckin()).getSosHome());
  }

  /**
   * Test {@link SOS#getSosServerPath()}.
   * <p>
   * Method under test: {@link SOS#getSosServerPath()}
   */
  @Test
  public void testGetSosServerPath() {
    // Arrange, Act and Assert
    assertNull((new SOSCheckin()).getSosServerPath());
  }

  /**
   * Test {@link SOS#getFilename()}.
   * <p>
   * Method under test: {@link SOS#getFilename()}
   */
  @Test
  public void testGetFilename() {
    // Arrange, Act and Assert
    assertNull((new SOSCheckin()).getFilename());
  }

  /**
   * Test {@link SOS#getNoCompress()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) NoCompress is {@code true}.</li>
   *   <li>Then return {@link SOSCmd#FLAG_NO_COMPRESSION}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getNoCompress()}
   */
  @Test
  public void testGetNoCompress_givenSOSCheckinNoCompressIsTrue_thenReturnFlag_no_compression() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setNoCompress(true);

    // Act and Assert
    assertEquals(SOSCmd.FLAG_NO_COMPRESSION, sosCheckin.getNoCompress());
  }

  /**
   * Test {@link SOS#getNoCompress()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getNoCompress()}
   */
  @Test
  public void testGetNoCompress_givenSOSCheckin_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new SOSCheckin()).getNoCompress());
  }

  /**
   * Test {@link SOS#getNoCache()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) NoCache is {@code true}.</li>
   *   <li>Then return {@link SOSCmd#FLAG_NO_CACHE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getNoCache()}
   */
  @Test
  public void testGetNoCache_givenSOSCheckinNoCacheIsTrue_thenReturnFlag_no_cache() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setNoCache(true);

    // Act and Assert
    assertEquals(SOSCmd.FLAG_NO_CACHE, sosCheckin.getNoCache());
  }

  /**
   * Test {@link SOS#getNoCache()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getNoCache()}
   */
  @Test
  public void testGetNoCache_givenSOSCheckin_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new SOSCheckin()).getNoCache());
  }

  /**
   * Test {@link SOS#getVerbose()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) Verbose is {@code true}.</li>
   *   <li>Then return {@link SOSCmd#FLAG_VERBOSE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getVerbose()}
   */
  @Test
  public void testGetVerbose_givenSOSCheckinVerboseIsTrue_thenReturnFlag_verbose() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setVerbose(true);

    // Act and Assert
    assertEquals(SOSCmd.FLAG_VERBOSE, sosCheckin.getVerbose());
  }

  /**
   * Test {@link SOS#getVerbose()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getVerbose()}
   */
  @Test
  public void testGetVerbose_givenSOSCheckin_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new SOSCheckin()).getVerbose());
  }

  /**
   * Test {@link SOS#getRecursive()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) InternalRecursive is {@code true}.</li>
   *   <li>Then return {@link SOSCmd#FLAG_RECURSION}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getRecursive()}
   */
  @Test
  public void testGetRecursive_givenSOSCheckinInternalRecursiveIsTrue_thenReturnFlag_recursion() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setInternalRecursive(true);

    // Act and Assert
    assertEquals(SOSCmd.FLAG_RECURSION, sosCheckin.getRecursive());
  }

  /**
   * Test {@link SOS#getRecursive()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getRecursive()}
   */
  @Test
  public void testGetRecursive_givenSOSCheckin_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new SOSCheckin()).getRecursive());
  }

  /**
   * Test {@link SOS#getLocalPath()}.
   * <p>
   * Method under test: {@link SOS#getLocalPath()}
   */
  @Test
  public void testGetLocalPath() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(project);

    // Act
    String actualLocalPath = sosCheckin.getLocalPath();

    // Assert
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), actualLocalPath);
  }

  /**
   * Test {@link SOS#getLocalPath()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getLocalPath()}
   */
  @Test
  public void testGetLocalPath_givenJavaLangObject_thenReturnPropertyIsUserDir() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Users", typeClass);
    project.addBuildListener(new AntClassLoader());

    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(project);

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), sosCheckin.getLocalPath());
  }

  /**
   * Test {@link SOS#getLocalPath()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getLocalPath()}
   */
  @Test
  public void testGetLocalPath_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(project);

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), sosCheckin.getLocalPath());
  }

  /**
   * Test {@link SOS#getLocalPath()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#getLocalPath()}
   */
  @Test
  public void testGetLocalPath_givenSOSCheckinProjectIsProject_thenReturnPropertyIsUserDir() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(new Project());

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), sosCheckin.getLocalPath());
  }

  /**
   * Test {@link SOS#execute()}.
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(project);
    sosCheckin.setProjectPath(SOSCmd.FLAG_COMMAND);
    sosCheckin.setVssServerPath(SOSCmd.FLAG_COMMAND);
    sosCheckin.setUsername("janedoe");
    sosCheckin.setSosServerPath("windows");

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(project);
    sosCheckin.setProjectPath(SOSCmd.FLAG_COMMAND);
    sosCheckin.setVssServerPath(SOSCmd.FLAG_COMMAND);
    sosCheckin.setUsername("janedoe");
    sosCheckin.setSosServerPath("windows");

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) InternalRecursive is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenSOSCheckinInternalRecursiveIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setInternalRecursive(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) Password is {@code iloveyou}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenSOSCheckinPasswordIsIloveyou_thenThrowBuildException() throws BuildException {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setPassword("iloveyou");
    sosCheckin.setUsername("janedoe");
    sosCheckin.setSosServerPath("windows");

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenSOSCheckinProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(new Project());
    sosCheckin.setProjectPath(SOSCmd.FLAG_COMMAND);
    sosCheckin.setVssServerPath(SOSCmd.FLAG_COMMAND);
    sosCheckin.setUsername("janedoe");
    sosCheckin.setSosServerPath("windows");

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) SosCmd is {@code windows}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenSOSCheckinSosCmdIsWindows_thenThrowBuildException() throws BuildException {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setSosCmd("windows");

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) SosServerPath is {@code windows}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenSOSCheckinSosServerPathIsWindows_thenThrowBuildException() throws BuildException {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setSosServerPath("windows");

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) Username is {@code janedoe}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenSOSCheckinUsernameIsJanedoe_thenThrowBuildException() throws BuildException {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setUsername("janedoe");
    sosCheckin.setSosServerPath("windows");

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor) VssServerPath is {@link SOSCmd#FLAG_COMMAND}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenSOSCheckinVssServerPathIsFlag_command_thenThrowBuildException() throws BuildException {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setVssServerPath(SOSCmd.FLAG_COMMAND);
    sosCheckin.setUsername("janedoe");
    sosCheckin.setSosServerPath("windows");

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.execute());
  }

  /**
   * Test {@link SOS#execute()}.
   * <ul>
   *   <li>Given {@link SOSCheckin} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#execute()}
   */
  @Test
  public void testExecute_givenSOSCheckin_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SOSCheckin()).execute());
  }

  /**
   * Test {@link SOS#run(Commandline)}.
   * <p>
   * Method under test: {@link SOS#run(Commandline)}
   */
  @Test
  public void testRun() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    project.addBuildListener(new AntClassLoader());

    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.run(new Commandline("To Process")));
  }

  /**
   * Test {@link SOS#run(Commandline)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#run(Commandline)}
   */
  @Test
  public void testRun_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.run(new Commandline("To Process")));
  }

  /**
   * Test {@link SOS#run(Commandline)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#run(Commandline)}
   */
  @Test
  public void testRun_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.run(new Commandline("To Process")));
  }

  /**
   * Test {@link SOS#run(Commandline)}.
   * <ul>
   *   <li>When {@link Commandline#Commandline(String)} with toProcess is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#run(Commandline)}
   */
  @Test
  public void testRun_whenCommandlineWithToProcessIsEmptyString_thenThrowBuildException() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.run(new Commandline("")));
  }

  /**
   * Test {@link SOS#run(Commandline)}.
   * <ul>
   *   <li>When {@link Commandline#Commandline(String)} with {@code To Process}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOS#run(Commandline)}
   */
  @Test
  public void testRun_whenCommandlineWithToProcess_thenThrowBuildException() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();
    sosCheckin.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> sosCheckin.run(new Commandline("To Process")));
  }
}
