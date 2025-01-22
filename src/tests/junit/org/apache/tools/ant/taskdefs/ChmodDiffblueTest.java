package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.ExecuteOn.FileDirBoth;
import org.apache.tools.ant.types.AbstractFileSet;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.PatternSet;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.LineOrientedOutputStreamRedirector;
import org.junit.Test;

public class ChmodDiffblueTest {
  /**
   * Test new {@link Chmod} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Chmod}
   */
  @Test
  public void testNewChmod() {
    // Arrange and Act
    Chmod actualChmod = new Chmod();

    // Assert
    assertNull(actualChmod.destDir);
    assertNull(actualChmod.getDescription());
    assertNull(actualChmod.getTaskName());
    assertNull(actualChmod.getTaskType());
    assertNull(actualChmod.getOs());
    assertNull(actualChmod.getOsFamily());
    assertNull(actualChmod.getProject());
    assertNull(actualChmod.getOwningTarget());
    assertNull(actualChmod.srcFilePos);
    assertNull(actualChmod.targetFilePos);
    assertNull(actualChmod.mapperElement);
    assertNull(actualChmod.redirectorElement);
    assertNull(actualChmod.mapper);
    assertFalse(actualChmod.getResolveExecutable());
    assertFalse(actualChmod.failOnError);
    assertFalse(actualChmod.newEnvironment);
    assertTrue(actualChmod.filesets.isEmpty());
    assertTrue(actualChmod.isValidOs());
    assertTrue(actualChmod.srcIsFirst);
    assertEquals(FileDirBoth.FILE, actualChmod.type);
  }

  /**
   * Test {@link Chmod#setProject(Project)}.
   * <p>
   * Method under test: {@link Chmod#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    Chmod chmod = new Chmod();
    Project project = new Project();

    // Act
    chmod.setProject(project);

    // Assert
    assertSame(project, chmod.getProject());
  }

  /**
   * Test {@link Chmod#setFile(File)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecuteOn#filesets} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#setFile(File)}
   */
  @Test
  public void testSetFile_whenNull_file_placeholder_thenChmodFilesetsSizeIsOne() {
    // Arrange
    Chmod chmod = new Chmod();

    // Act
    chmod.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    Vector<AbstractFileSet> abstractFileSetList = chmod.filesets;
    assertEquals(1, abstractFileSetList.size());
    AbstractFileSet getResult = abstractFileSetList.get(0);
    assertTrue(getResult instanceof FileSet);
    assertNull(getResult.getDescription());
    assertNull(getResult.getProject());
    assertNull(getResult.getRefid());
    assertEquals(5, getResult.getMaxLevelsOfSymlinks());
    assertFalse(getResult.isReference());
    assertTrue(getResult.getDefaultexcludes());
    assertTrue(getResult.getErrorOnMissingDir());
    assertTrue(((FileSet) getResult).isFilesystemOnly());
  }

  /**
   * Test {@link Chmod#setPerm(String)}.
   * <p>
   * Method under test: {@link Chmod#setPerm(String)}
   */
  @Test
  public void testSetPerm() {
    // Arrange
    Chmod chmod = new Chmod();

    // Act
    chmod.setPerm("Perm");

    // Assert
    Commandline commandline = chmod.cmdl;
    String[] commandline2 = commandline.getCommandline();
    assertEquals("Perm", commandline2[1]);
    Iterator<Argument> iteratorResult = commandline.iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(2, commandline.size());
    assertEquals(2, commandline2.length);
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"Perm"}, commandline.getArguments());
    assertArrayEquals(new String[]{"Perm"}, nextResult.getParts());
  }

  /**
   * Test {@link Chmod#createInclude()}.
   * <p>
   * Method under test: {@link Chmod#createInclude()}
   */
  @Test
  public void testCreateInclude() {
    // Arrange, Act and Assert
    assertNull((new Chmod()).createInclude().getName());
  }

  /**
   * Test {@link Chmod#createExclude()}.
   * <p>
   * Method under test: {@link Chmod#createExclude()}
   */
  @Test
  public void testCreateExclude() {
    // Arrange, Act and Assert
    assertNull((new Chmod()).createExclude().getName());
  }

  /**
   * Test {@link Chmod#createPatternSet()}.
   * <p>
   * Method under test: {@link Chmod#createPatternSet()}
   */
  @Test
  public void testCreatePatternSet() {
    // Arrange and Act
    PatternSet actualCreatePatternSetResult = (new Chmod()).createPatternSet();

    // Assert
    Location location = actualCreatePatternSetResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePatternSetResult.getDescription());
    assertNull(actualCreatePatternSetResult.getProject());
    assertNull(actualCreatePatternSetResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreatePatternSetResult.isReference());
  }

  /**
   * Test {@link Chmod#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecuteOn#mapper} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenChmodAddFilelistFileList_thenChmodMapperIsNull() {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.addFilelist(new FileList());
    chmod.setPerm("Required attribute perm not set in chmod");

    // Act
    chmod.checkConfiguration();

    // Assert that nothing has changed
    assertNull(chmod.mapper);
  }

  /**
   * Test {@link Chmod#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecuteOn#mapper} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenChmodAddFilesetFileSet_thenChmodMapperIsNull() {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.addFileset(new FileSet());
    chmod.setPerm("Required attribute perm not set in chmod");

    // Act
    chmod.checkConfiguration();

    // Assert that nothing has changed
    assertNull(chmod.mapper);
  }

  /**
   * Test {@link Chmod#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) Spawn is {@code true}.</li>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecuteOn#mapper} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenChmodSpawnIsTrue_thenChmodMapperIsNull() {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setSpawn(true);
    chmod.addFileset(new FileSet());
    chmod.setPerm("Required attribute perm not set in chmod");

    // Act
    chmod.checkConfiguration();

    // Assert that nothing has changed
    assertNull(chmod.mapper);
  }

  /**
   * Test {@link Chmod#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) TaskName is {@code execon}.</li>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecuteOn#mapper} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenChmodTaskNameIsExecon_thenChmodMapperIsNull() {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setTaskName("execon");
    chmod.addFileset(new FileSet());
    chmod.setPerm("Required attribute perm not set in chmod");

    // Act
    chmod.checkConfiguration();

    // Assert that nothing has changed
    assertNull(chmod.mapper);
  }

  /**
   * Test {@link Chmod#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenChmod_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chmod()).checkConfiguration());
  }

  /**
   * Test {@link Chmod#checkConfiguration()}.
   * <ul>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecuteOn#mapper} Mappers size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_thenChmodMapperMappersSizeIsOne() throws BuildException {
    // Arrange
    Chmod chmod = new Chmod();
    CutDirsMapper fileNameMapper = new CutDirsMapper();
    chmod.add(fileNameMapper);
    chmod.addFileset(new FileSet());
    chmod.setPerm("Required attribute perm not set in chmod");

    // Act
    chmod.checkConfiguration();

    // Assert
    FileNameMapper fileNameMapper2 = chmod.mapper;
    List<FileNameMapper> mappers = ((CompositeMapper) fileNameMapper2).getMappers();
    assertEquals(1, mappers.size());
    FileNameMapper getResult = mappers.get(0);
    assertTrue(getResult instanceof CutDirsMapper);
    assertTrue(fileNameMapper2 instanceof CompositeMapper);
    assertSame(fileNameMapper, getResult);
    FileNameMapper expectedImplementation = chmod.mapper;
    assertSame(expectedImplementation, chmod.mapperElement.getImplementation());
  }

  /**
   * Test {@link Chmod#checkConfiguration()}.
   * <ul>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecuteOn#mapper} Mappers size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_thenChmodMapperMappersSizeIsOne2() throws BuildException {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setDest(Copy.NULL_FILE_PLACEHOLDER);
    CutDirsMapper fileNameMapper = new CutDirsMapper();
    chmod.add(fileNameMapper);
    chmod.addFileset(new FileSet());
    chmod.setPerm("Required attribute perm not set in chmod");

    // Act
    chmod.checkConfiguration();

    // Assert
    FileNameMapper fileNameMapper2 = chmod.mapper;
    List<FileNameMapper> mappers = ((CompositeMapper) fileNameMapper2).getMappers();
    assertEquals(1, mappers.size());
    FileNameMapper getResult = mappers.get(0);
    assertTrue(getResult instanceof CutDirsMapper);
    assertTrue(fileNameMapper2 instanceof CompositeMapper);
    assertSame(fileNameMapper, getResult);
    FileNameMapper expectedImplementation = chmod.mapper;
    assertSame(expectedImplementation, chmod.mapperElement.getImplementation());
  }

  /**
   * Test {@link Chmod#execute()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) addFilelist {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#execute()}
   */
  @Test
  public void testExecute_givenChmodAddFilelistNull() throws BuildException {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setProject(new Project());
    chmod.addFilelist(null);
    chmod.setPerm("unix");

    // Act
    chmod.execute();

    // Assert
    Redirector redirector = chmod.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link Chmod#execute()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) OsFamily is {@code unix}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#execute()}
   */
  @Test
  public void testExecute_givenChmodOsFamilyIsUnix_thenThrowBuildException() throws BuildException {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setOsFamily("unix");

    // Act and Assert
    assertThrows(BuildException.class, () -> chmod.execute());
  }

  /**
   * Test {@link Chmod#execute()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) OsFamily is {@code windows}.</li>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecTask#redirector} ErrorStream is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#execute()}
   */
  @Test
  public void testExecute_givenChmodOsFamilyIsWindows_thenChmodRedirectorErrorStreamIsNull() throws BuildException {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setOsFamily("windows");

    // Act
    chmod.execute();

    // Assert that nothing has changed
    Redirector redirector = chmod.redirector;
    assertNull(redirector.getErrorStream());
    assertNull(redirector.getOutputStream());
  }

  /**
   * Test {@link Chmod#execute()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) OsFamily is {@code windows}.</li>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecTask#redirector} ErrorStream is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#execute()}
   */
  @Test
  public void testExecute_givenChmodOsFamilyIsWindows_thenChmodRedirectorErrorStreamIsNull2() throws BuildException {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setDir(Copy.NULL_FILE_PLACEHOLDER);
    chmod.setOsFamily("windows");

    // Act
    chmod.execute();

    // Assert that nothing has changed
    Redirector redirector = chmod.redirector;
    assertNull(redirector.getErrorStream());
    assertNull(redirector.getOutputStream());
  }

  /**
   * Test {@link Chmod#execute()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) Os is {@code unix}.</li>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecTask#redirector} ErrorStream is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#execute()}
   */
  @Test
  public void testExecute_givenChmodOsIsUnix_thenChmodRedirectorErrorStreamIsNull() throws BuildException {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setOs("unix");

    // Act
    chmod.execute();

    // Assert that nothing has changed
    Redirector redirector = chmod.redirector;
    assertNull(redirector.getErrorStream());
    assertNull(redirector.getOutputStream());
  }

  /**
   * Test {@link Chmod#execute()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#execute()}
   */
  @Test
  public void testExecute_givenChmod_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chmod()).execute());
  }

  /**
   * Test {@link Chmod#execute()}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code windows}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#execute()}
   */
  @Test
  public void testExecute_givenFileNameNameIsWindows() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("windows");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Chmod chmod = new Chmod();
    chmod.setProject(new Project());
    chmod.addFilelist(list);
    chmod.setPerm("unix");

    // Act
    chmod.execute();

    // Assert
    Redirector redirector = chmod.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link Chmod#execute()}.
   * <ul>
   *   <li>Then {@link Chmod} (default constructor) {@link ExecTask#redirector} ErrorStream {@link LineOrientedOutputStreamRedirector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#execute()}
   */
  @Test
  public void testExecute_thenChmodRedirectorErrorStreamLineOrientedOutputStreamRedirector() throws BuildException {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setProject(new Project());
    chmod.addFilelist(new FileList());
    chmod.setPerm("unix");

    // Act
    chmod.execute();

    // Assert
    Redirector redirector = chmod.redirector;
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link Chmod#setExecutable(String)}.
   * <p>
   * Method under test: {@link Chmod#setExecutable(String)}
   */
  @Test
  public void testSetExecutable() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chmod()).setExecutable("foo"));
  }

  /**
   * Test {@link Chmod#setCommand(Commandline)}.
   * <p>
   * Method under test: {@link Chmod#setCommand(Commandline)}
   */
  @Test
  public void testSetCommand() {
    // Arrange
    Chmod chmod = new Chmod();

    // Act and Assert
    assertThrows(BuildException.class, () -> chmod.setCommand(new Commandline("To Process")));
  }

  /**
   * Test {@link Chmod#setSkipEmptyFilesets(boolean)}.
   * <p>
   * Method under test: {@link Chmod#setSkipEmptyFilesets(boolean)}
   */
  @Test
  public void testSetSkipEmptyFilesets() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chmod()).setSkipEmptyFilesets(true));
  }

  /**
   * Test {@link Chmod#setAddsourcefile(boolean)}.
   * <p>
   * Method under test: {@link Chmod#setAddsourcefile(boolean)}
   */
  @Test
  public void testSetAddsourcefile() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chmod()).setAddsourcefile(true));
  }

  /**
   * Test {@link Chmod#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) OsFamily is {@code unix}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChmodOsFamilyIsUnix_thenReturnTrue() {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setOsFamily("unix");

    // Act and Assert
    assertTrue(chmod.isValidOs());
  }

  /**
   * Test {@link Chmod#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) OsFamily is {@code windows}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChmodOsFamilyIsWindows_thenReturnFalse() {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setOsFamily("windows");

    // Act and Assert
    assertFalse(chmod.isValidOs());
  }

  /**
   * Test {@link Chmod#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor) Os is {@code unix}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChmodOsIsUnix_thenReturnFalse() {
    // Arrange
    Chmod chmod = new Chmod();
    chmod.setOs("unix");

    // Act and Assert
    assertFalse(chmod.isValidOs());
  }

  /**
   * Test {@link Chmod#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chmod} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chmod#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChmod_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Chmod()).isValidOs());
  }
}
