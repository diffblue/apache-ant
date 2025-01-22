package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.List;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.ExecuteOn.FileDirBoth;
import org.apache.tools.ant.types.AbstractFileSet;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Marker;
import org.apache.tools.ant.types.DirSet;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.RedirectorElement;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.optional.depend.DependScanner;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.LineOrientedOutputStreamRedirector;
import org.apache.tools.ant.util.NullOutputStream;
import org.junit.Test;

public class ExecuteOnDiffblueTest {
  /**
   * Test {@link ExecuteOn#addFileset(FileSet)}.
   * <p>
   * Method under test: {@link ExecuteOn#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    FileSet set = new FileSet();

    // Act
    executeOn.addFileset(set);

    // Assert
    Vector<AbstractFileSet> abstractFileSetList = executeOn.filesets;
    assertEquals(1, abstractFileSetList.size());
    assertSame(set, abstractFileSetList.get(0));
  }

  /**
   * Test {@link ExecuteOn#addDirset(DirSet)}.
   * <p>
   * Method under test: {@link ExecuteOn#addDirset(DirSet)}
   */
  @Test
  public void testAddDirset() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    DirSet set = new DirSet();

    // Act
    executeOn.addDirset(set);

    // Assert
    Vector<AbstractFileSet> abstractFileSetList = executeOn.filesets;
    assertEquals(1, abstractFileSetList.size());
    assertSame(set, abstractFileSetList.get(0));
  }

  /**
   * Test {@link ExecuteOn#add(FileNameMapper)} with {@code fileNameMapper}.
   * <p>
   * Method under test: {@link ExecuteOn#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    CutDirsMapper fileNameMapper = new CutDirsMapper();

    // Act
    executeOn.add(fileNameMapper);

    // Assert
    Mapper mapper = executeOn.mapperElement;
    FileNameMapper implementation = mapper.getImplementation();
    assertTrue(implementation instanceof CompositeMapper);
    assertNull(mapper.getDescription());
    assertNull(mapper.getProject());
    assertNull(mapper.getRefid());
    List<FileNameMapper> mappers = ((CompositeMapper) implementation).getMappers();
    assertEquals(1, mappers.size());
    assertFalse(mapper.isReference());
    assertSame(fileNameMapper, mappers.get(0));
  }

  /**
   * Test {@link ExecuteOn#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_givenExecuteOnAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.add(new CutDirsMapper()));
  }

  /**
   * Test FileDirBoth {@link FileDirBoth#getValues()}.
   * <p>
   * Method under test: {@link FileDirBoth#getValues()}
   */
  @Test
  public void testFileDirBothGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{FileDirBoth.FILE, FileDirBoth.DIR, "both"}, (new FileDirBoth()).getValues());
  }

  /**
   * Test FileDirBoth new {@link FileDirBoth} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FileDirBoth}
   */
  @Test
  public void testFileDirBothNewFileDirBoth() {
    // Arrange and Act
    FileDirBoth actualFileDirBoth = new FileDirBoth();

    // Assert
    assertNull(actualFileDirBoth.getValue());
    assertEquals(-1, actualFileDirBoth.getIndex());
  }

  /**
   * Test {@link ExecuteOn#setType(FileDirBoth)}.
   * <ul>
   *   <li>When {@link FileDirBoth} (default constructor).</li>
   *   <li>Then {@link ExecuteOn} (default constructor) {@link ExecuteOn#type} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#setType(FileDirBoth)}
   */
  @Test
  public void testSetType_whenFileDirBoth_thenExecuteOnTypeIsNull() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();

    // Act
    executeOn.setType(new FileDirBoth());

    // Assert
    assertNull(executeOn.type);
  }

  /**
   * Test {@link ExecuteOn#createSrcfile()}.
   * <p>
   * Method under test: {@link ExecuteOn#createSrcfile()}
   */
  @Test
  public void testCreateSrcfile() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();

    // Act
    Marker actualCreateSrcfileResult = executeOn.createSrcfile();

    // Assert
    Marker marker = executeOn.srcFilePos;
    assertEquals("", marker.getPrefix());
    assertEquals("", marker.getSuffix());
    assertEquals(0, marker.getPosition());
    assertSame(executeOn.srcFilePos, actualCreateSrcfileResult);
  }

  /**
   * Test {@link ExecuteOn#createTargetfile()}.
   * <p>
   * Method under test: {@link ExecuteOn#createTargetfile()}
   */
  @Test
  public void testCreateTargetfile() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();

    // Act
    Marker actualCreateTargetfileResult = executeOn.createTargetfile();

    // Assert
    Marker marker = executeOn.targetFilePos;
    assertEquals("", marker.getPrefix());
    assertEquals("", marker.getSuffix());
    assertEquals(0, marker.getPosition());
    assertFalse(executeOn.srcIsFirst);
    assertSame(executeOn.targetFilePos, actualCreateTargetfileResult);
  }

  /**
   * Test {@link ExecuteOn#createMapper()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#createMapper()}
   */
  @Test
  public void testCreateMapper_givenExecuteOnAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.createMapper());
  }

  /**
   * Test {@link ExecuteOn#createMapper()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor).</li>
   *   <li>Then {@link ExecuteOn} (default constructor) {@link ExecuteOn#mapperElement} Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#createMapper()}
   */
  @Test
  public void testCreateMapper_givenExecuteOn_thenExecuteOnMapperElementDescriptionIsNull() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();

    // Act
    Mapper actualCreateMapperResult = executeOn.createMapper();

    // Assert
    Mapper mapper = executeOn.mapperElement;
    assertNull(mapper.getDescription());
    assertNull(mapper.getProject());
    assertNull(mapper.getRefid());
    assertFalse(mapper.isReference());
    assertSame(executeOn.mapperElement, actualCreateMapperResult);
  }

  /**
   * Test {@link ExecuteOn#checkConfiguration()}.
   * <p>
   * Method under test: {@link ExecuteOn#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    executeOn.addFileset(new FileSet());
    executeOn.setCommand(new Commandline("execon"));

    // Act
    executeOn.checkConfiguration();

    // Assert that nothing has changed
    assertNull(executeOn.mapper);
  }

  /**
   * Test {@link ExecuteOn#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenExecuteOnAddFilelistFileList() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.addFilelist(new FileList());
    executeOn.setCommand(new Commandline("execon"));

    // Act
    executeOn.checkConfiguration();

    // Assert that nothing has changed
    assertNull(executeOn.mapper);
  }

  /**
   * Test {@link ExecuteOn#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then {@link ExecuteOn} (default constructor) {@link ExecuteOn#mapper} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenExecuteOnAddFilesetFileSet_thenExecuteOnMapperIsNull() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.addFileset(new FileSet());
    executeOn.setCommand(new Commandline("execon"));

    // Act
    executeOn.checkConfiguration();

    // Assert that nothing has changed
    assertNull(executeOn.mapper);
  }

  /**
   * Test {@link ExecuteOn#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) Spawn is {@code true}.</li>
   *   <li>Then {@link ExecuteOn} (default constructor) {@link ExecuteOn#mapper} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenExecuteOnSpawnIsTrue_thenExecuteOnMapperIsNull() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setSpawn(true);
    executeOn.addFileset(new FileSet());
    executeOn.setCommand(new Commandline("execon"));

    // Act
    executeOn.checkConfiguration();

    // Assert that nothing has changed
    assertNull(executeOn.mapper);
  }

  /**
   * Test {@link ExecuteOn#checkConfiguration()}.
   * <ul>
   *   <li>Then {@link ExecuteOn} (default constructor) {@link ExecuteOn#mapper} Mappers size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_thenExecuteOnMapperMappersSizeIsOne() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    CutDirsMapper fileNameMapper = new CutDirsMapper();
    executeOn.add(fileNameMapper);
    executeOn.addFileset(new FileSet());
    executeOn.setCommand(new Commandline("execon"));

    // Act
    executeOn.checkConfiguration();

    // Assert
    FileNameMapper fileNameMapper2 = executeOn.mapper;
    List<FileNameMapper> mappers = ((CompositeMapper) fileNameMapper2).getMappers();
    assertEquals(1, mappers.size());
    FileNameMapper getResult = mappers.get(0);
    assertTrue(getResult instanceof CutDirsMapper);
    assertTrue(fileNameMapper2 instanceof CompositeMapper);
    assertSame(fileNameMapper, getResult);
    FileNameMapper expectedImplementation = executeOn.mapper;
    assertSame(expectedImplementation, executeOn.mapperElement.getImplementation());
  }

  /**
   * Test {@link ExecuteOn#checkConfiguration()}.
   * <ul>
   *   <li>Then {@link ExecuteOn} (default constructor) {@link ExecuteOn#mapper} Mappers size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_thenExecuteOnMapperMappersSizeIsOne2() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setDest(Copy.NULL_FILE_PLACEHOLDER);
    CutDirsMapper fileNameMapper = new CutDirsMapper();
    executeOn.add(fileNameMapper);
    executeOn.addFileset(new FileSet());
    executeOn.setCommand(new Commandline("execon"));

    // Act
    executeOn.checkConfiguration();

    // Assert
    FileNameMapper fileNameMapper2 = executeOn.mapper;
    List<FileNameMapper> mappers = ((CompositeMapper) fileNameMapper2).getMappers();
    assertEquals(1, mappers.size());
    FileNameMapper getResult = mappers.get(0);
    assertTrue(getResult instanceof CutDirsMapper);
    assertTrue(fileNameMapper2 instanceof CompositeMapper);
    assertSame(fileNameMapper, getResult);
    FileNameMapper expectedImplementation = executeOn.mapper;
    assertSame(expectedImplementation, executeOn.mapperElement.getImplementation());
  }

  /**
   * Test {@link ExecuteOn#checkConfiguration()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_thenThrowBuildException() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setCommand(new Commandline("execon"));

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.checkConfiguration());
  }

  /**
   * Test {@link ExecuteOn#checkConfiguration()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_thenThrowBuildException2() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setDest(Copy.NULL_FILE_PLACEHOLDER);
    executeOn.addFileset(new FileSet());
    executeOn.setCommand(new Commandline("execon"));

    // Act and Assert
    assertThrows(BuildException.class, () -> executeOn.checkConfiguration());
  }

  /**
   * Test {@link ExecuteOn#createHandler()}.
   * <p>
   * Method under test: {@link ExecuteOn#createHandler()}
   */
  @Test
  public void testCreateHandler() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("Discarding output"));

    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(project);
    executeOn.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = executeOn.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecuteOn#createHandler()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) addConfiguredRedirector {@link RedirectorElement} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#createHandler()}
   */
  @Test
  public void testCreateHandler_givenExecuteOnAddConfiguredRedirectorRedirectorElement() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.addConfiguredRedirector(new RedirectorElement());

    // Act and Assert
    assertTrue(executeOn.createHandler() instanceof PumpStreamHandler);
  }

  /**
   * Test {@link ExecuteOn#createHandler()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) DiscardError is {@code true}.</li>
   *   <li>Then Out return {@link LogOutputStream}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#createHandler()}
   */
  @Test
  public void testCreateHandler_givenExecuteOnDiscardErrorIsTrue_thenOutReturnLogOutputStream() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setDiscardError(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = executeOn.createHandler();

    // Assert
    OutputStream out = ((PumpStreamHandler) actualCreateHandlerResult).getOut();
    assertTrue(out instanceof LogOutputStream);
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof NullOutputStream);
    assertEquals(2, ((LogOutputStream) out).getMessageLevel());
  }

  /**
   * Test {@link ExecuteOn#createHandler()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#createHandler()}
   */
  @Test
  public void testCreateHandler_givenExecuteOnProjectIsProject() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(new Project());
    executeOn.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = executeOn.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecuteOn#createHandler()}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor).</li>
   *   <li>Then Err return {@link LineOrientedOutputStreamRedirector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#createHandler()}
   */
  @Test
  public void testCreateHandler_givenExecuteOn_thenErrReturnLineOrientedOutputStreamRedirector() throws BuildException {
    // Arrange and Act
    ExecuteStreamHandler actualCreateHandlerResult = (new ExecuteOn()).createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecuteOn#createHandler()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#createHandler()}
   */
  @Test
  public void testCreateHandler_givenJavaLangObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(project);
    executeOn.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = executeOn.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecuteOn#createHandler()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#createHandler()}
   */
  @Test
  public void testCreateHandler_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setProject(project);
    executeOn.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = executeOn.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecuteOn#createHandler()}.
   * <ul>
   *   <li>Then Err return {@link LineOrientedOutputStreamRedirector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#createHandler()}
   */
  @Test
  public void testCreateHandler_thenErrReturnLineOrientedOutputStreamRedirector() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = executeOn.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link ExecuteOn#getCommandline(String, File)} with {@code srcFile}, {@code baseDir}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) Forwardslash is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#getCommandline(String, File)}
   */
  @Test
  public void testGetCommandlineWithSrcFileBaseDir_givenExecuteOnForwardslashIsTrue() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setForwardslash(true);

    // Act and Assert
    assertArrayEquals(new String[]{"/NULL_FILE/Src File"},
        executeOn.getCommandline("Src File", Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link ExecuteOn#getCommandline(String, File)} with {@code srcFile}, {@code baseDir}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code /NULL_FILE/Src File}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#getCommandline(String, File)}
   */
  @Test
  public void testGetCommandlineWithSrcFileBaseDir_thenReturnArrayOfStringWithNullFileSrcFile() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"/NULL_FILE/Src File"},
        (new ExecuteOn()).getCommandline("Src File", Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link ExecuteOn#getCommandline(String, File)} with {@code srcFile}, {@code baseDir}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code Src File}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#getCommandline(String, File)}
   */
  @Test
  public void testGetCommandlineWithSrcFileBaseDir_thenReturnArrayOfStringWithSrcFile() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setRelative(true);

    // Act and Assert
    assertArrayEquals(new String[]{"Src File"}, executeOn.getCommandline("Src File", Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link ExecuteOn#getCommandline(String, File)} with {@code srcFile}, {@code baseDir}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code To} and {@code Process}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#getCommandline(String, File)}
   */
  @Test
  public void testGetCommandlineWithSrcFileBaseDir_thenReturnArrayOfStringWithToAndProcess() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setCommand(new Commandline("To Process"));

    // Act and Assert
    assertArrayEquals(new String[]{"To", "Process", "/NULL_FILE/Src File"},
        executeOn.getCommandline("Src File", Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link ExecuteOn#getCommandline(String[], File[])} with {@code srcFiles}, {@code baseDirs}.
   * <p>
   * Method under test: {@link ExecuteOn#getCommandline(String[], File[])}
   */
  @Test
  public void testGetCommandlineWithSrcFilesBaseDirs() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"/NULL_FILE/Src Files"},
        (new ExecuteOn()).getCommandline(new String[]{"Src Files"}, new File[]{Copy.NULL_FILE_PLACEHOLDER}));
  }

  /**
   * Test {@link ExecuteOn#getCommandline(String[], File[])} with {@code srcFiles}, {@code baseDirs}.
   * <ul>
   *   <li>Given {@link ExecuteOn} (default constructor) Forwardslash is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#getCommandline(String[], File[])}
   */
  @Test
  public void testGetCommandlineWithSrcFilesBaseDirs_givenExecuteOnForwardslashIsTrue() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setForwardslash(true);

    // Act and Assert
    assertArrayEquals(new String[]{"/NULL_FILE/Src Files"},
        executeOn.getCommandline(new String[]{"Src Files"}, new File[]{Copy.NULL_FILE_PLACEHOLDER}));
  }

  /**
   * Test {@link ExecuteOn#getCommandline(String[], File[])} with {@code srcFiles}, {@code baseDirs}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code Src Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#getCommandline(String[], File[])}
   */
  @Test
  public void testGetCommandlineWithSrcFilesBaseDirs_thenReturnArrayOfStringWithSrcFiles() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setRelative(true);

    // Act and Assert
    assertArrayEquals(new String[]{"Src Files"},
        executeOn.getCommandline(new String[]{"Src Files"}, new File[]{Copy.NULL_FILE_PLACEHOLDER}));
  }

  /**
   * Test {@link ExecuteOn#getCommandline(String[], File[])} with {@code srcFiles}, {@code baseDirs}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code To} and {@code Process}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#getCommandline(String[], File[])}
   */
  @Test
  public void testGetCommandlineWithSrcFilesBaseDirs_thenReturnArrayOfStringWithToAndProcess() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();
    executeOn.setCommand(new Commandline("To Process"));

    // Act and Assert
    assertArrayEquals(new String[]{"To", "Process", "/NULL_FILE/Src Files"},
        executeOn.getCommandline(new String[]{"Src Files"}, new File[]{Copy.NULL_FILE_PLACEHOLDER}));
  }

  /**
   * Test {@link ExecuteOn#getDirs(File, DirectoryScanner)}.
   * <ul>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#getDirs(File, DirectoryScanner)}
   */
  @Test
  public void testGetDirs_thenReturnArrayLengthIsZero() {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();

    // Act and Assert
    assertEquals(0, executeOn.getDirs(Copy.NULL_FILE_PLACEHOLDER, new DependScanner(new DirectoryScanner())).length);
  }

  /**
   * Test {@link ExecuteOn#getFilesAndDirs(FileList)}.
   * <ul>
   *   <li>Then return array of {@link String} with {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteOn#getFilesAndDirs(FileList)}
   */
  @Test
  public void testGetFilesAndDirs_thenReturnArrayOfStringWithAttribute_name() throws BuildException {
    // Arrange
    ExecuteOn executeOn = new ExecuteOn();

    FileName name = new FileName();
    name.setName(Manifest.ATTRIBUTE_NAME);

    FileList list = new FileList();
    list.addConfiguredFile(name);
    list.setDir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertArrayEquals(new String[]{Manifest.ATTRIBUTE_NAME}, executeOn.getFilesAndDirs(list));
  }

  /**
   * Test new {@link ExecuteOn} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ExecuteOn}
   */
  @Test
  public void testNewExecuteOn() {
    // Arrange and Act
    ExecuteOn actualExecuteOn = new ExecuteOn();

    // Assert
    assertNull(actualExecuteOn.destDir);
    assertNull(actualExecuteOn.getDescription());
    assertNull(actualExecuteOn.getTaskName());
    assertNull(actualExecuteOn.getTaskType());
    assertNull(actualExecuteOn.getOs());
    assertNull(actualExecuteOn.getOsFamily());
    assertNull(actualExecuteOn.getProject());
    assertNull(actualExecuteOn.getOwningTarget());
    assertNull(actualExecuteOn.srcFilePos);
    assertNull(actualExecuteOn.targetFilePos);
    assertNull(actualExecuteOn.mapperElement);
    assertNull(actualExecuteOn.redirectorElement);
    assertNull(actualExecuteOn.mapper);
    assertFalse(actualExecuteOn.getResolveExecutable());
    assertFalse(actualExecuteOn.failOnError);
    assertFalse(actualExecuteOn.newEnvironment);
    assertTrue(actualExecuteOn.filesets.isEmpty());
    assertTrue(actualExecuteOn.srcIsFirst);
    assertEquals(FileDirBoth.FILE, actualExecuteOn.type);
  }
}
