package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.Environment;
import org.apache.tools.ant.types.Environment.Variable;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.RedirectorElement;
import org.junit.Test;

public class AbstractJarSignerTaskDiffblueTest {
  /**
   * Test {@link AbstractJarSignerTask#setMaxmemory(String)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setMaxmemory(String)}
   */
  @Test
  public void testSetMaxmemory() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setMaxmemory("Max");

    // Assert
    assertEquals("Max", signJar.maxMemory);
  }

  /**
   * Test {@link AbstractJarSignerTask#setJar(File)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setJar(File)}
   */
  @Test
  public void testSetJar() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setJar(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    File file = signJar.jar;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link AbstractJarSignerTask#setAlias(String)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setAlias(String)}
   */
  @Test
  public void testSetAlias() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setAlias("Alias");

    // Assert
    assertEquals("Alias", signJar.alias);
  }

  /**
   * Test {@link AbstractJarSignerTask#setKeystore(String)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setKeystore(String)}
   */
  @Test
  public void testSetKeystore() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setKeystore("Keystore");

    // Assert
    assertEquals("Keystore", signJar.keystore);
  }

  /**
   * Test {@link AbstractJarSignerTask#setStorepass(String)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setStorepass(String)}
   */
  @Test
  public void testSetStorepass() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setStorepass("Storepass");

    // Assert
    assertEquals("Storepass", signJar.storepass);
  }

  /**
   * Test {@link AbstractJarSignerTask#setStoretype(String)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setStoretype(String)}
   */
  @Test
  public void testSetStoretype() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setStoretype("Storetype");

    // Assert
    assertEquals("Storetype", signJar.storetype);
  }

  /**
   * Test {@link AbstractJarSignerTask#setKeypass(String)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setKeypass(String)}
   */
  @Test
  public void testSetKeypass() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setKeypass("Keypass");

    // Assert
    assertEquals("Keypass", signJar.keypass);
  }

  /**
   * Test {@link AbstractJarSignerTask#setVerbose(boolean)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setVerbose(boolean)}
   */
  @Test
  public void testSetVerbose() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setVerbose(true);

    // Assert
    assertTrue(signJar.verbose);
  }

  /**
   * Test {@link AbstractJarSignerTask#setStrict(boolean)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setStrict(boolean)}
   */
  @Test
  public void testSetStrict() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.setStrict(true);

    // Assert
    assertTrue(signJar.strict);
  }

  /**
   * Test {@link AbstractJarSignerTask#addFileset(FileSet)}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset() {
    // Arrange
    SignJar signJar = new SignJar();
    FileSet set = new FileSet();

    // Act
    signJar.addFileset(set);

    // Assert
    Vector<FileSet> fileSetList = signJar.filesets;
    assertEquals(1, fileSetList.size());
    assertTrue(signJar.hasResources());
    assertSame(set, fileSetList.get(0));
  }

  /**
   * Test {@link AbstractJarSignerTask#createPath()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createPath()}
   */
  @Test
  public void testCreatePath_givenSignJarProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    SignJar signJar = new SignJar();
    Project project = new Project();
    signJar.setProject(project);

    // Act and Assert
    assertSame(project, signJar.createPath().getProject());
  }

  /**
   * Test {@link AbstractJarSignerTask#createPath()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createPath()}
   */
  @Test
  public void testCreatePath_givenSignJar_thenReturnLocationFileNameIsNull() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    Path actualCreatePathResult = signJar.createPath();

    // Assert
    Location location = actualCreatePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePathResult.getDescription());
    assertNull(actualCreatePathResult.getProject());
    assertNull(actualCreatePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreatePathResult.size());
    assertFalse(actualCreatePathResult.isReference());
    assertTrue(signJar.hasResources());
    assertTrue(actualCreatePathResult.isEmpty());
  }

  /**
   * Test {@link AbstractJarSignerTask#beginExecution()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#beginExecution()}
   */
  @Test
  public void testBeginExecution_givenSignJar() {
    // Arrange
    SignJar signJar = new SignJar();

    // Act
    signJar.beginExecution();

    // Assert
    RedirectorElement redirector = signJar.getRedirector();
    assertNull(redirector.getDescription());
    assertNull(redirector.getProject());
    assertNull(redirector.getRefid());
    assertFalse(redirector.isReference());
  }

  /**
   * Test {@link AbstractJarSignerTask#beginExecution()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Keypass is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#beginExecution()}
   */
  @Test
  public void testBeginExecution_givenSignJarKeypassIsFoo() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setStorepass("foo");
    signJar.setKeypass("foo");

    // Act
    signJar.beginExecution();

    // Assert
    RedirectorElement redirector = signJar.getRedirector();
    assertNull(redirector.getDescription());
    assertNull(redirector.getProject());
    assertNull(redirector.getRefid());
    assertFalse(redirector.isReference());
  }

  /**
   * Test {@link AbstractJarSignerTask#beginExecution()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Keypass is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#beginExecution()}
   */
  @Test
  public void testBeginExecution_givenSignJarKeypassIsNull() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setStorepass("foo");
    signJar.setKeypass(null);

    // Act
    signJar.beginExecution();

    // Assert
    RedirectorElement redirector = signJar.getRedirector();
    assertNull(redirector.getDescription());
    assertNull(redirector.getProject());
    assertNull(redirector.getRefid());
    assertFalse(redirector.isReference());
  }

  /**
   * Test {@link AbstractJarSignerTask#getRedirector()}.
   * <p>
   * Method under test: {@link AbstractJarSignerTask#getRedirector()}
   */
  @Test
  public void testGetRedirector() {
    // Arrange, Act and Assert
    assertNull((new SignJar()).getRedirector());
  }

  /**
   * Test {@link AbstractJarSignerTask#setCommonOptions(ExecTask)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>Then not {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} iterator hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setCommonOptions(ExecTask)}
   */
  @Test
  public void testSetCommonOptions_givenSignJar_thenNotExecTaskCmdlIteratorHasNext() {
    // Arrange
    SignJar signJar = new SignJar();
    ExecTask cmd = new ExecTask();

    // Act
    signJar.setCommonOptions(cmd);

    // Assert that nothing has changed
    assertFalse(cmd.cmdl.iterator().hasNext());
  }

  /**
   * Test {@link AbstractJarSignerTask#setCommonOptions(ExecTask)}.
   * <ul>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} iterator next Parts is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#setCommonOptions(ExecTask)}
   */
  @Test
  public void testSetCommonOptions_thenExecTaskCmdlIteratorNextPartsIsNull() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.addArg(new Argument());
    ExecTask cmd = new ExecTask();

    // Act
    signJar.setCommonOptions(cmd);

    // Assert
    Iterator<Argument> iteratorResult = cmd.cmdl.iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getParts());
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertFalse(iteratorResult.hasNext());
  }

  /**
   * Test {@link AbstractJarSignerTask#declareSysProperty(ExecTask, Variable)}.
   * <ul>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} iterator next Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#declareSysProperty(ExecTask, Variable)}
   */
  @Test
  public void testDeclareSysProperty_thenExecTaskCmdlIteratorNextDescriptionIsNull() throws BuildException {
    // Arrange
    SignJar signJar = new SignJar();
    ExecTask cmd = new ExecTask();

    Variable property = new Variable();
    property.setKey("Property");
    property.setValue("Property");

    // Act
    signJar.declareSysProperty(cmd, property);

    // Assert
    Commandline commandline = cmd.cmdl;
    Iterator<Argument> iteratorResult = commandline.iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(1, commandline.size());
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"-J-DProperty=Property"}, commandline.getArguments());
    assertArrayEquals(new String[]{"-J-DProperty=Property"}, commandline.getCommandline());
    assertArrayEquals(new String[]{"-J-DProperty=Property"}, nextResult.getParts());
  }

  /**
   * Test {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}
   */
  @Test
  public void testBindToKeystore_givenJavaLangObject_thenExecTaskCmdlSizeIsZero() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    SignJar signJar = new SignJar();
    signJar.setProject(project);
    signJar.setKeystore(null);
    signJar.setStoretype(null);
    signJar.setProviderName(null);
    signJar.setProviderClass(null);
    signJar.setProviderArg("foo");
    ExecTask cmd = new ExecTask();

    // Act
    signJar.bindToKeystore(cmd);

    // Assert that nothing has changed
    Commandline commandline = cmd.cmdl;
    assertEquals(0, commandline.size());
    assertEquals(0, commandline.getArguments().length);
    assertEquals(0, commandline.getCommandline().length);
    assertFalse(commandline.iterator().hasNext());
  }

  /**
   * Test {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}
   */
  @Test
  public void testBindToKeystore_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    SignJar signJar = new SignJar();
    signJar.setProject(project);
    signJar.setKeystore(null);
    signJar.setStoretype(null);
    signJar.setProviderName(null);
    signJar.setProviderClass(null);
    signJar.setProviderArg("foo");
    ExecTask cmd = new ExecTask();

    // Act
    signJar.bindToKeystore(cmd);

    // Assert that nothing has changed
    Commandline commandline = cmd.cmdl;
    assertEquals(0, commandline.size());
    assertEquals(0, commandline.getArguments().length);
    assertEquals(0, commandline.getCommandline().length);
    assertFalse(commandline.iterator().hasNext());
  }

  /**
   * Test {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}
   */
  @Test
  public void testBindToKeystore_givenSignJarProjectIsProject_thenExecTaskCmdlSizeIsZero() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setProject(new Project());
    signJar.setKeystore(null);
    signJar.setStoretype(null);
    signJar.setProviderName(null);
    signJar.setProviderClass(null);
    signJar.setProviderArg("foo");
    ExecTask cmd = new ExecTask();

    // Act
    signJar.bindToKeystore(cmd);

    // Assert that nothing has changed
    Commandline commandline = cmd.cmdl;
    assertEquals(0, commandline.size());
    assertEquals(0, commandline.getArguments().length);
    assertEquals(0, commandline.getCommandline().length);
    assertFalse(commandline.iterator().hasNext());
  }

  /**
   * Test {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) ProviderArg is {@code foo}.</li>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}
   */
  @Test
  public void testBindToKeystore_givenSignJarProviderArgIsFoo_thenExecTaskCmdlSizeIsZero() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setKeystore(null);
    signJar.setStoretype(null);
    signJar.setProviderName(null);
    signJar.setProviderClass(null);
    signJar.setProviderArg("foo");
    ExecTask cmd = new ExecTask();

    // Act
    signJar.bindToKeystore(cmd);

    // Assert that nothing has changed
    Commandline commandline = cmd.cmdl;
    assertEquals(0, commandline.size());
    assertEquals(0, commandline.getArguments().length);
    assertEquals(0, commandline.getCommandline().length);
    assertFalse(commandline.iterator().hasNext());
  }

  /**
   * Test {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) ProviderClass is {@code foo}.</li>
   *   <li>Then third element is {@code -providerArg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}
   */
  @Test
  public void testBindToKeystore_givenSignJarProviderClassIsFoo_thenThirdElementIsProviderArg() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setKeystore(null);
    signJar.setStoretype(null);
    signJar.setProviderName(null);
    signJar.setProviderClass("foo");
    signJar.setProviderArg("foo");
    ExecTask cmd = new ExecTask();

    // Act
    signJar.bindToKeystore(cmd);

    // Assert
    Commandline commandline = cmd.cmdl;
    String[] arguments = commandline.getArguments();
    assertEquals("-providerArg", arguments[2]);
    String[] commandline2 = commandline.getCommandline();
    assertEquals("-providerArg", commandline2[2]);
    assertEquals("foo", arguments[3]);
    assertEquals("foo", commandline2[3]);
    Iterator<Argument> iteratorResult = commandline.iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(1, nextResult.getParts().length);
    assertEquals(4, commandline.size());
    assertEquals(4, arguments.length);
    assertEquals(4, commandline2.length);
    assertTrue(iteratorResult.hasNext());
  }

  /**
   * Test {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#bindToKeystore(ExecTask)}
   */
  @Test
  public void testBindToKeystore_givenSignJar_thenExecTaskCmdlSizeIsZero() {
    // Arrange
    SignJar signJar = new SignJar();
    ExecTask cmd = new ExecTask();

    // Act
    signJar.bindToKeystore(cmd);

    // Assert that nothing has changed
    Commandline commandline = cmd.cmdl;
    assertEquals(0, commandline.size());
    assertEquals(0, commandline.getArguments().length);
    assertEquals(0, commandline.getCommandline().length);
    assertFalse(commandline.iterator().hasNext());
  }

  /**
   * Test {@link AbstractJarSignerTask#createJarSigner()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>Then return {@link ExecTask#cmdl} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createJarSigner()}
   */
  @Test
  public void testCreateJarSigner_givenSignJar_thenReturnCmdlSizeIsOne() {
    // Arrange, Act and Assert
    Commandline commandline = (new SignJar()).createJarSigner().cmdl;
    assertEquals(1, commandline.size());
    String expectedExecutable = Paths.get(System.getProperty("java.home"), "bin", "jarsigner").toString();
    assertEquals(expectedExecutable, commandline.getExecutable());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.home"), "bin", "jarsigner").toString()},
        commandline.getCommandline());
  }

  /**
   * Test {@link AbstractJarSignerTask#createJarSigner()}.
   * <ul>
   *   <li>Then return TaskType is {@code jarsigner}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createJarSigner()}
   */
  @Test
  public void testCreateJarSigner_thenReturnTaskTypeIsJarsigner() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setExecutable("");

    // Act
    ExecTask actualCreateJarSignerResult = signJar.createJarSigner();

    // Assert
    assertEquals("jarsigner", actualCreateJarSignerResult.getTaskType());
    assertNull(actualCreateJarSignerResult.getDescription());
    assertNull(actualCreateJarSignerResult.getTaskName());
    assertNull(actualCreateJarSignerResult.getOs());
    assertNull(actualCreateJarSignerResult.getOsFamily());
    assertNull(actualCreateJarSignerResult.getProject());
    assertNull(actualCreateJarSignerResult.getOwningTarget());
    assertNull(actualCreateJarSignerResult.redirectorElement);
    assertFalse(actualCreateJarSignerResult.getResolveExecutable());
    assertFalse(actualCreateJarSignerResult.newEnvironment);
    assertTrue(actualCreateJarSignerResult.failOnError);
  }

  /**
   * Test {@link AbstractJarSignerTask#createUnifiedSources()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createUnifiedSources()}
   */
  @Test
  public void testCreateUnifiedSources_givenSignJar_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new SignJar()).createUnifiedSources().isEmpty());
  }

  /**
   * Test {@link AbstractJarSignerTask#createUnifiedSources()}.
   * <ul>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createUnifiedSources()}
   */
  @Test
  public void testCreateUnifiedSources_thenReturnSizeIsOne() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setJar(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    Vector<FileSet> actualCreateUnifiedSourcesResult = signJar.createUnifiedSources();

    // Assert
    assertEquals(1, actualCreateUnifiedSourcesResult.size());
    FileSet getResult = actualCreateUnifiedSourcesResult.get(0);
    File dir = getResult.getDir();
    assertEquals("", dir.getName());
    Location location = getResult.getLocation();
    assertNull(location.getFileName());
    assertNull(getResult.getDescription());
    assertNull(getResult.getProject());
    assertNull(getResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(5, getResult.getMaxLevelsOfSymlinks());
    assertFalse(getResult.isReference());
    assertTrue(dir.isAbsolute());
    assertTrue(getResult.getDefaultexcludes());
    assertTrue(getResult.getErrorOnMissingDir());
    assertTrue(getResult.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractJarSignerTask#createUnifiedSourcePath()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createUnifiedSourcePath()}
   */
  @Test
  public void testCreateUnifiedSourcePath_givenSignJarAddFilesetFileSet() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.addFileset(new FileSet());

    // Act
    Path actualCreateUnifiedSourcePathResult = signJar.createUnifiedSourcePath();

    // Assert
    Location location = actualCreateUnifiedSourcePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateUnifiedSourcePathResult.getDescription());
    assertNull(actualCreateUnifiedSourcePathResult.getProject());
    assertNull(actualCreateUnifiedSourcePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreateUnifiedSourcePathResult.isReference());
  }

  /**
   * Test {@link AbstractJarSignerTask#createUnifiedSourcePath()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createUnifiedSourcePath()}
   */
  @Test
  public void testCreateUnifiedSourcePath_givenSignJarAddFilesetFileSet2() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.addFileset(new FileSet());
    signJar.addFileset(new FileSet());

    // Act
    Path actualCreateUnifiedSourcePathResult = signJar.createUnifiedSourcePath();

    // Assert
    Location location = actualCreateUnifiedSourcePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateUnifiedSourcePathResult.getDescription());
    assertNull(actualCreateUnifiedSourcePathResult.getProject());
    assertNull(actualCreateUnifiedSourcePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreateUnifiedSourcePathResult.isReference());
  }

  /**
   * Test {@link AbstractJarSignerTask#createUnifiedSourcePath()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) addFileset {@code null}.</li>
   *   <li>Then return size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createUnifiedSourcePath()}
   */
  @Test
  public void testCreateUnifiedSourcePath_givenSignJarAddFilesetNull_thenReturnSizeIsZero() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.addFileset(null);

    // Act
    Path actualCreateUnifiedSourcePathResult = signJar.createUnifiedSourcePath();

    // Assert
    Location location = actualCreateUnifiedSourcePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateUnifiedSourcePathResult.getDescription());
    assertNull(actualCreateUnifiedSourcePathResult.getProject());
    assertNull(actualCreateUnifiedSourcePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateUnifiedSourcePathResult.size());
    assertFalse(actualCreateUnifiedSourcePathResult.isReference());
    assertTrue(actualCreateUnifiedSourcePathResult.isEmpty());
  }

  /**
   * Test {@link AbstractJarSignerTask#createUnifiedSourcePath()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) Jar is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createUnifiedSourcePath()}
   */
  @Test
  public void testCreateUnifiedSourcePath_givenSignJarJarIsNull_file_placeholder() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.setJar(Copy.NULL_FILE_PLACEHOLDER);
    signJar.addFileset(new FileSet());

    // Act
    Path actualCreateUnifiedSourcePathResult = signJar.createUnifiedSourcePath();

    // Assert
    Location location = actualCreateUnifiedSourcePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateUnifiedSourcePathResult.getDescription());
    assertNull(actualCreateUnifiedSourcePathResult.getProject());
    assertNull(actualCreateUnifiedSourcePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreateUnifiedSourcePathResult.isReference());
  }

  /**
   * Test {@link AbstractJarSignerTask#createUnifiedSourcePath()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>Then return size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createUnifiedSourcePath()}
   */
  @Test
  public void testCreateUnifiedSourcePath_givenSignJar_thenReturnSizeIsZero() {
    // Arrange and Act
    Path actualCreateUnifiedSourcePathResult = (new SignJar()).createUnifiedSourcePath();

    // Assert
    Location location = actualCreateUnifiedSourcePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateUnifiedSourcePathResult.getDescription());
    assertNull(actualCreateUnifiedSourcePathResult.getProject());
    assertNull(actualCreateUnifiedSourcePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateUnifiedSourcePathResult.size());
    assertFalse(actualCreateUnifiedSourcePathResult.isReference());
    assertTrue(actualCreateUnifiedSourcePathResult.isEmpty());
  }

  /**
   * Test {@link AbstractJarSignerTask#createUnifiedSourcePath()}.
   * <ul>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#createUnifiedSourcePath()}
   */
  @Test
  public void testCreateUnifiedSourcePath_thenReturnProjectIsProject() {
    // Arrange
    SignJar signJar = new SignJar();
    Project project = new Project();
    signJar.setProject(project);
    signJar.addFileset(new FileSet());

    // Act and Assert
    assertSame(project, signJar.createUnifiedSourcePath().getProject());
  }

  /**
   * Test {@link AbstractJarSignerTask#hasResources()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#hasResources()}
   */
  @Test
  public void testHasResources_givenSignJarAddFilesetFileSet_thenReturnTrue() {
    // Arrange
    SignJar signJar = new SignJar();
    signJar.addFileset(new FileSet());

    // Act and Assert
    assertTrue(signJar.hasResources());
  }

  /**
   * Test {@link AbstractJarSignerTask#hasResources()}.
   * <ul>
   *   <li>Given {@link SignJar} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#hasResources()}
   */
  @Test
  public void testHasResources_givenSignJar_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new SignJar()).hasResources());
  }

  /**
   * Test {@link AbstractJarSignerTask#addValue(ExecTask, String)}.
   * <ul>
   *   <li>When {@link ExecTask#ExecTask()}.</li>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} iterator next Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#addValue(ExecTask, String)}
   */
  @Test
  public void testAddValue_whenExecTask_thenExecTaskCmdlIteratorNextDescriptionIsNull() {
    // Arrange
    SignJar signJar = new SignJar();
    ExecTask cmd = new ExecTask();

    // Act
    signJar.addValue(cmd, "42");

    // Assert
    Commandline commandline = cmd.cmdl;
    Iterator<Argument> iteratorResult = commandline.iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(1, commandline.size());
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"42"}, commandline.getArguments());
    assertArrayEquals(new String[]{"42"}, commandline.getCommandline());
    assertArrayEquals(new String[]{"42"}, nextResult.getParts());
  }

  /**
   * Test {@link AbstractJarSignerTask#addArgument(ExecTask, Argument)}.
   * <ul>
   *   <li>When {@link ExecTask#ExecTask()}.</li>
   *   <li>Then {@link ExecTask#ExecTask()} {@link ExecTask#cmdl} iterator next Parts is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractJarSignerTask#addArgument(ExecTask, Argument)}
   */
  @Test
  public void testAddArgument_whenExecTask_thenExecTaskCmdlIteratorNextPartsIsNull() {
    // Arrange
    SignJar signJar = new SignJar();
    ExecTask cmd = new ExecTask();

    // Act
    signJar.addArgument(cmd, new Argument());

    // Assert
    Iterator<Argument> iteratorResult = cmd.cmdl.iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getParts());
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertFalse(iteratorResult.hasNext());
  }
}
