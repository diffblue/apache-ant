package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.List;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.IdentityMapper;
import org.junit.Test;

public class ExpandDiffblueTest {
  /**
   * Test {@link Expand#Expand()}.
   * <p>
   * Method under test: {@link Expand#Expand()}
   */
  @Test
  public void testNewExpand() {
    // Arrange and Act
    Expand actualExpand = new Expand();

    // Assert
    assertTrue(actualExpand.getMapper() instanceof IdentityMapper);
    assertEquals("UTF8", actualExpand.getEncoding());
    assertNull(actualExpand.getAllowFilesToEscapeDest());
    assertNull(actualExpand.getDescription());
    assertNull(actualExpand.getTaskName());
    assertNull(actualExpand.getTaskType());
    assertNull(actualExpand.getProject());
    assertNull(actualExpand.getOwningTarget());
    assertFalse(actualExpand.getFailOnEmptyArchive());
    assertTrue(actualExpand.getScanForUnicodeExtraFields());
  }

  /**
   * Test {@link Expand#Expand(String)}.
   * <p>
   * Method under test: {@link Expand#Expand(String)}
   */
  @Test
  public void testNewExpand2() {
    // Arrange and Act
    Expand actualExpand = new Expand(Manifest.JAR_ENCODING);

    // Assert
    assertTrue(actualExpand.getMapper() instanceof IdentityMapper);
    assertNull(actualExpand.getAllowFilesToEscapeDest());
    assertNull(actualExpand.getDescription());
    assertNull(actualExpand.getTaskName());
    assertNull(actualExpand.getTaskType());
    assertNull(actualExpand.getProject());
    assertNull(actualExpand.getOwningTarget());
    assertFalse(actualExpand.getFailOnEmptyArchive());
    assertTrue(actualExpand.getScanForUnicodeExtraFields());
    assertEquals(Manifest.JAR_ENCODING, actualExpand.getEncoding());
  }

  /**
   * Test {@link Expand#execute()}.
   * <p>
   * Method under test: {@link Expand#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Expand expand = new Expand();
    expand.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    expand.add(Path.systemBootClasspath);
    expand.setDest(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    expand.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> expand.execute());
  }

  /**
   * Test {@link Expand#execute()}.
   * <ul>
   *   <li>Given {@link Expand#Expand()} addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#execute()}
   */
  @Test
  public void testExecute_givenExpandAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Expand expand = new Expand();
    expand.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> expand.execute());
  }

  /**
   * Test {@link Expand#execute()}.
   * <ul>
   *   <li>Given {@link Expand#Expand()} Src is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#execute()}
   */
  @Test
  public void testExecute_givenExpandSrcIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Expand expand = new Expand();
    expand.setSrc(Copy.NULL_FILE_PLACEHOLDER);
    expand.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> expand.execute());
  }

  /**
   * Test {@link Expand#execute()}.
   * <ul>
   *   <li>Given {@link Expand#Expand()} Src is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#execute()}
   */
  @Test
  public void testExecute_givenExpandSrcIsNull_file_placeholder_thenThrowBuildException2() throws BuildException {
    // Arrange
    Expand expand = new Expand();
    expand.setSrc(Copy.NULL_FILE_PLACEHOLDER);
    expand.add(Path.systemBootClasspath);
    expand.setDest(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    expand.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> expand.execute());
  }

  /**
   * Test {@link Expand#execute()}.
   * <ul>
   *   <li>Given {@link Expand#Expand()} TaskType is {@code expand}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#execute()}
   */
  @Test
  public void testExecute_givenExpandTaskTypeIsExpand_thenThrowBuildException() throws BuildException {
    // Arrange
    Expand expand = new Expand();
    expand.setTaskType("expand");

    // Act and Assert
    assertThrows(BuildException.class, () -> expand.execute());
  }

  /**
   * Test {@link Expand#execute()}.
   * <ul>
   *   <li>Given {@link Expand#Expand()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#execute()}
   */
  @Test
  public void testExecute_givenExpand_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Expand()).execute());
  }

  /**
   * Test {@link Expand#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Expand#Expand()} add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenExpandAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    Expand expand = new Expand();
    expand.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> expand.expandFile(FileUtils.getFileUtils(), Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Expand#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Expand#Expand()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenExpandProjectIsProject_thenThrowBuildException() {
    // Arrange
    Expand expand = new Expand();
    expand.setProject(new Project());
    expand.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> expand.expandFile(FileUtils.getFileUtils(), Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Expand#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Expand#Expand()}.</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenExpand_whenNull_file_placeholder_thenThrowBuildException() {
    // Arrange
    Expand expand = new Expand();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> expand.expandFile(FileUtils.getFileUtils(), Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Expand#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Expand#Expand()}.</li>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenExpand_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    Expand expand = new Expand();
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertThrows(BuildException.class, () -> expand.expandFile(fileUtils,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Expand#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(" into ", typeClass);
    project.addBuildListener(new AntClassLoader());

    Expand expand = new Expand();
    expand.setProject(project);
    expand.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> expand.expandFile(FileUtils.getFileUtils(), Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Expand#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Expand expand = new Expand();
    expand.setProject(project);
    expand.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> expand.expandFile(FileUtils.getFileUtils(), Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Expand#expandResource(Resource, File)}.
   * <ul>
   *   <li>Given {@link Expand#Expand()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_givenExpand_whenResource_thenThrowBuildException() {
    // Arrange
    Expand expand = new Expand();

    // Act and Assert
    assertThrows(BuildException.class, () -> expand.expandResource(new Resource(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Expand#getMapper()}.
   * <ul>
   *   <li>Given {@link Expand#Expand()} add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return Mappers size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#getMapper()}
   */
  @Test
  public void testGetMapper_givenExpandAddCutDirsMapper_thenReturnMappersSizeIsOne() {
    // Arrange
    Expand expand = new Expand();
    CutDirsMapper fileNameMapper = new CutDirsMapper();
    expand.add(fileNameMapper);

    // Act
    FileNameMapper actualMapper = expand.getMapper();

    // Assert
    List<FileNameMapper> mappers = ((CompositeMapper) actualMapper).getMappers();
    assertEquals(1, mappers.size());
    FileNameMapper getResult = mappers.get(0);
    assertTrue(getResult instanceof CutDirsMapper);
    assertTrue(actualMapper instanceof CompositeMapper);
    assertSame(fileNameMapper, getResult);
  }

  /**
   * Test {@link Expand#getMapper()}.
   * <ul>
   *   <li>Given {@link Expand#Expand()}.</li>
   *   <li>Then return {@link IdentityMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#getMapper()}
   */
  @Test
  public void testGetMapper_givenExpand_thenReturnIdentityMapper() {
    // Arrange and Act
    FileNameMapper actualMapper = (new Expand()).getMapper();

    // Assert
    assertTrue(actualMapper instanceof IdentityMapper);
    assertArrayEquals(new String[]{"foo.txt"}, actualMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link Expand#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Given {@link Expand#Expand()} add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_givenExpandAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    Expand expand = new Expand();
    expand.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> expand.add(new CutDirsMapper()));
  }

  /**
   * Test {@link Expand#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Given {@link Expand#Expand()}.</li>
   *   <li>Then {@link Expand#Expand()} Mapper {@link CompositeMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_givenExpand_thenExpandMapperCompositeMapper() {
    // Arrange
    Expand expand = new Expand();
    CutDirsMapper fileNameMapper = new CutDirsMapper();

    // Act
    expand.add(fileNameMapper);

    // Assert
    FileNameMapper mapper = expand.getMapper();
    assertTrue(mapper instanceof CompositeMapper);
    List<FileNameMapper> mappers = ((CompositeMapper) mapper).getMappers();
    assertEquals(1, mappers.size());
    assertSame(fileNameMapper, mappers.get(0));
  }

  /**
   * Test {@link Expand#setEncoding(String)}.
   * <ul>
   *   <li>When {@link Manifest#JAR_ENCODING}.</li>
   *   <li>Then {@link Expand#Expand()} Encoding is {@link Manifest#JAR_ENCODING}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#setEncoding(String)}
   */
  @Test
  public void testSetEncoding_whenJar_encoding_thenExpandEncodingIsJar_encoding() {
    // Arrange
    Expand expand = new Expand();

    // Act
    expand.setEncoding(Manifest.JAR_ENCODING);

    // Assert
    assertEquals(Manifest.JAR_ENCODING, expand.getEncoding());
  }

  /**
   * Test {@link Expand#setEncoding(String)}.
   * <ul>
   *   <li>When {@link Expand#NATIVE_ENCODING}.</li>
   *   <li>Then {@link Expand#Expand()} Encoding is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#setEncoding(String)}
   */
  @Test
  public void testSetEncoding_whenNative_encoding_thenExpandEncodingIsNull() {
    // Arrange
    Expand expand = new Expand();

    // Act
    expand.setEncoding(Expand.NATIVE_ENCODING);

    // Assert
    assertNull(expand.getEncoding());
  }

  /**
   * Test {@link Expand#internalSetEncoding(String)}.
   * <ul>
   *   <li>When {@link Manifest#JAR_ENCODING}.</li>
   *   <li>Then {@link Expand#Expand()} Encoding is {@link Manifest#JAR_ENCODING}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#internalSetEncoding(String)}
   */
  @Test
  public void testInternalSetEncoding_whenJar_encoding_thenExpandEncodingIsJar_encoding() {
    // Arrange
    Expand expand = new Expand();

    // Act
    expand.internalSetEncoding(Manifest.JAR_ENCODING);

    // Assert
    assertEquals(Manifest.JAR_ENCODING, expand.getEncoding());
  }

  /**
   * Test {@link Expand#internalSetEncoding(String)}.
   * <ul>
   *   <li>When {@link Expand#NATIVE_ENCODING}.</li>
   *   <li>Then {@link Expand#Expand()} Encoding is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#internalSetEncoding(String)}
   */
  @Test
  public void testInternalSetEncoding_whenNative_encoding_thenExpandEncodingIsNull() {
    // Arrange
    Expand expand = new Expand();

    // Act
    expand.internalSetEncoding(Expand.NATIVE_ENCODING);

    // Assert
    assertNull(expand.getEncoding());
  }

  /**
   * Test {@link Expand#setAllowFilesToEscapeDest(boolean)}.
   * <p>
   * Method under test: {@link Expand#setAllowFilesToEscapeDest(boolean)}
   */
  @Test
  public void testSetAllowFilesToEscapeDest() {
    // Arrange
    Expand expand = new Expand();

    // Act
    expand.setAllowFilesToEscapeDest(true);

    // Assert
    assertTrue(expand.getAllowFilesToEscapeDest());
  }

  /**
   * Test {@link Expand#createMapper()}.
   * <ul>
   *   <li>Given {@link Expand#Expand()} add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#createMapper()}
   */
  @Test
  public void testCreateMapper_givenExpandAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    Expand expand = new Expand();
    expand.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> expand.createMapper());
  }

  /**
   * Test {@link Expand#createMapper()}.
   * <ul>
   *   <li>Given {@link Expand#Expand()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Expand#createMapper()}
   */
  @Test
  public void testCreateMapper_givenExpand_thenReturnLocationFileNameIsNull() throws BuildException {
    // Arrange and Act
    Mapper actualCreateMapperResult = (new Expand()).createMapper();

    // Assert
    Location location = actualCreateMapperResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateMapperResult.getDescription());
    assertNull(actualCreateMapperResult.getProject());
    assertNull(actualCreateMapperResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreateMapperResult.isReference());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Expand#internalSetScanForUnicodeExtraFields(boolean)}
   *   <li>{@link Expand#setDest(File)}
   *   <li>{@link Expand#setFailOnEmptyArchive(boolean)}
   *   <li>{@link Expand#setOverwrite(boolean)}
   *   <li>{@link Expand#setSrc(File)}
   *   <li>{@link Expand#setStripAbsolutePathSpec(boolean)}
   *   <li>{@link Expand#getAllowFilesToEscapeDest()}
   *   <li>{@link Expand#getEncoding()}
   *   <li>{@link Expand#getFailOnEmptyArchive()}
   *   <li>{@link Expand#getScanForUnicodeExtraFields()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Expand expand = new Expand();

    // Act
    expand.internalSetScanForUnicodeExtraFields(true);
    expand.setDest(Copy.NULL_FILE_PLACEHOLDER);
    expand.setFailOnEmptyArchive(true);
    expand.setOverwrite(true);
    expand.setSrc(Copy.NULL_FILE_PLACEHOLDER);
    expand.setStripAbsolutePathSpec(true);
    Boolean actualAllowFilesToEscapeDest = expand.getAllowFilesToEscapeDest();
    String actualEncoding = expand.getEncoding();
    boolean actualFailOnEmptyArchive = expand.getFailOnEmptyArchive();

    // Assert
    assertEquals("UTF8", actualEncoding);
    assertNull(actualAllowFilesToEscapeDest);
    assertTrue(actualFailOnEmptyArchive);
    assertTrue(expand.getScanForUnicodeExtraFields());
  }
}
