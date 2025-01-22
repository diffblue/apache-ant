package org.apache.tools.ant.taskdefs.optional.image;

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
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.UnknownElement;
import org.apache.tools.ant.types.AntFilterReader;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.util.ChainedMapper;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.MergingMapper;
import org.junit.Test;

public class ImageIOTaskDiffblueTest {
  /**
   * Test {@link ImageIOTask#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_thenThrowBuildException() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.add(new CutDirsMapper()));
  }

  /**
   * Test {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}.
   * <ul>
   *   <li>Given {@link AntFilterReader} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}
   */
  @Test
  public void testProcessDir_givenAntFilterReader() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File dstDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    FilterMapper mapper = new FilterMapper();
    mapper.addFilterReader(new AntFilterReader());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.processDir(srcDir, new String[]{"Src Names"}, dstDir, mapper));
  }

  /**
   * Test {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor) Gc is {@code true}.</li>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}
   */
  @Test
  public void testProcessDir_givenImageIOTaskGcIsTrue_whenEmptyArrayOfString_thenReturnZero() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setGc(true);
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File dstDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, imageIOTask.processDir(srcDir, new String[]{}, dstDir, new CutDirsMapper()));
  }

  /**
   * Test {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}
   */
  @Test
  public void testProcessDir_givenImageIOTaskProjectIsProject_thenThrowBuildException() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setProject(new Project());
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File dstDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processDir(srcDir, new String[]{"Src Names"}, dstDir, new ChainedMapper()));
  }

  /**
   * Test {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor).</li>
   *   <li>When array of {@link String} with empty string.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}
   */
  @Test
  public void testProcessDir_givenImageIOTask_whenArrayOfStringWithEmptyString_thenReturnZero() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File dstDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, imageIOTask.processDir(srcDir, new String[]{""}, dstDir, new ChainedMapper()));
  }

  /**
   * Test {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor).</li>
   *   <li>When {@link ChainedMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}
   */
  @Test
  public void testProcessDir_givenImageIOTask_whenChainedMapper_thenThrowBuildException() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File dstDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processDir(srcDir, new String[]{"Src Names"}, dstDir, new ChainedMapper()));
  }

  /**
   * Test {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor).</li>
   *   <li>When {@link CompositeMapper} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}
   */
  @Test
  public void testProcessDir_givenImageIOTask_whenCompositeMapper_thenReturnZero() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File dstDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, imageIOTask.processDir(srcDir, new String[]{"Src Names"}, dstDir, new CompositeMapper()));
  }

  /**
   * Test {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor).</li>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}
   */
  @Test
  public void testProcessDir_givenImageIOTask_whenEmptyArrayOfString_thenReturnZero() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File dstDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, imageIOTask.processDir(srcDir, new String[]{}, dstDir, new CutDirsMapper()));
  }

  /**
   * Test {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor).</li>
   *   <li>When {@link FilterMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}
   */
  @Test
  public void testProcessDir_givenImageIOTask_whenFilterMapper_thenThrowBuildException() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File dstDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processDir(srcDir, new String[]{"Src Names"}, dstDir, new FilterMapper()));
  }

  /**
   * Test {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}.
   * <ul>
   *   <li>When {@link MergingMapper#MergingMapper(String)} with to is {@code alice.liddell@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processDir(File, String[], File, FileNameMapper)}
   */
  @Test
  public void testProcessDir_whenMergingMapperWithToIsAliceLiddellExampleOrg() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    File srcDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File dstDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.processDir(srcDir, new String[]{"Src Names"}, dstDir,
        new MergingMapper("alice.liddell@example.org")));
  }

  /**
   * Test {@link ImageIOTask#processFile(File)} with {@code file}.
   * <p>
   * Method under test: {@link ImageIOTask#processFile(File)}
   */
  @Test
  public void testProcessFileWithFile() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setSrcdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ImageIOTask#processFile(File)} with {@code file}.
   * <p>
   * Method under test: {@link ImageIOTask#processFile(File)}
   */
  @Test
  public void testProcessFileWithFile2() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ImageIOTask#processFile(File)} with {@code file}.
   * <p>
   * Method under test: {@link ImageIOTask#processFile(File)}
   */
  @Test
  public void testProcessFileWithFile3() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setSrcdir(Paths.get(System.getProperty("java.io.tmpdir"), "").toFile());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ImageIOTask#processFile(File, File)} with {@code file}, {@code newFile}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processFile(File, File)}
   */
  @Test
  public void testProcessFileWithFileNewFile_givenImageIOTask_thenThrowBuildException() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processFile(file, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ImageIOTask#processFile(File, File)} with {@code file}, {@code newFile}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processFile(File, File)}
   */
  @Test
  public void testProcessFileWithFileNewFile_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setProject(project);
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processFile(file, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ImageIOTask#processFile(File, File)} with {@code file}, {@code newFile}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processFile(File, File)}
   */
  @Test
  public void testProcessFileWithFileNewFile_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setProject(project);
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processFile(file, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ImageIOTask#processFile(File, File)} with {@code file}, {@code newFile}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processFile(File, File)}
   */
  @Test
  public void testProcessFileWithFileNewFile_thenThrowBuildException() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setProject(new Project());
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> imageIOTask.processFile(file, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ImageIOTask#processFile(File, File)} with {@code file}, {@code newFile}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code Processing File:} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#processFile(File, File)}
   */
  @Test
  public void testProcessFileWithFileNewFile_whenPropertyIsJavaIoTmpdirIsProcessingFileToFile() {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.processFile(file,
        Paths.get(System.getProperty("java.io.tmpdir"), "Processing File: ").toFile()));
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setSrcdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    imageIOTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    imageIOTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute3() throws BuildException {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask
        .setSrcdir(Paths.get(System.getProperty("java.io.tmpdir"), ": Setup scanner in dir ", " with ").toFile());
    imageIOTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute4() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addReference("ant.ComponentHelper", new UnknownElement("No directory specified for %s."));
    project.addBuildListener(new AntClassLoader());

    FileSet set = new FileSet();
    set.setProject(project);
    set.appendSelector(new ScriptSelector());

    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    imageIOTask.addFileset(set);

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@link FileSet#FileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenFileSetAppendSelectorScriptSelector() throws BuildException {
    // Arrange
    FileSet set = new FileSet();
    set.appendSelector(new ScriptSelector());

    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    imageIOTask.addFileset(set);

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@link FileSet#FileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenFileSetProjectIsProject() throws BuildException {
    // Arrange
    FileSet set = new FileSet();
    set.setProject(new Project());
    set.appendSelector(new ScriptSelector());

    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    imageIOTask.addFileset(set);

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenImageIOTask() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ImageIOTask()).execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenImageIOTaskAddCutDirsMapper() throws BuildException {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.add(new CutDirsMapper());
    imageIOTask
        .setSrcdir(Paths.get(System.getProperty("java.io.tmpdir"), ": Setup scanner in dir ", " with ").toFile());
    imageIOTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenImageIOTaskAddFilesetFileSet() throws BuildException {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor) addFileset {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenImageIOTaskAddFilesetNull() throws BuildException {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    imageIOTask.addFileset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenImageIOTaskProjectIsProject() throws BuildException {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setProject(new Project());
    imageIOTask.add(new CutDirsMapper());
    imageIOTask
        .setSrcdir(Paths.get(System.getProperty("java.io.tmpdir"), ": Setup scanner in dir ", " with ").toFile());
    imageIOTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.ComponentHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    FileSet set = new FileSet();
    set.setProject(project);
    set.appendSelector(new ScriptSelector());

    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    imageIOTask.addFileset(set);

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    FileSet set = new FileSet();
    set.setProject(project);
    set.appendSelector(new ScriptSelector());

    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    imageIOTask.addFileset(set);

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.ComponentHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddReferenceAntComponentHelperAndValue() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addReference("ant.ComponentHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    FileSet set = new FileSet();
    set.setProject(project);
    set.appendSelector(new ScriptSelector());

    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    imageIOTask.addFileset(set);

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.execute());
  }

  /**
   * Test {@link ImageIOTask#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenImageIOTaskAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.validateAttributes());
  }

  /**
   * Test {@link ImageIOTask#validateAttributes()}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenImageIOTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ImageIOTask()).validateAttributes());
  }

  /**
   * Test {@link ImageIOTask#createMapper()}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#createMapper()}
   */
  @Test
  public void testCreateMapper_givenImageIOTaskAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    ImageIOTask imageIOTask = new ImageIOTask();
    imageIOTask.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> imageIOTask.createMapper());
  }

  /**
   * Test {@link ImageIOTask#createMapper()}.
   * <ul>
   *   <li>Given {@link ImageIOTask} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImageIOTask#createMapper()}
   */
  @Test
  public void testCreateMapper_givenImageIOTask_thenReturnLocationFileNameIsNull() throws BuildException {
    // Arrange and Act
    Mapper actualCreateMapperResult = (new ImageIOTask()).createMapper();

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
   * Test new {@link ImageIOTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ImageIOTask}
   */
  @Test
  public void testNewImageIOTask() {
    // Arrange and Act
    ImageIOTask actualImageIOTask = new ImageIOTask();

    // Assert
    Location location = actualImageIOTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualImageIOTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualImageIOTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualImageIOTask.getTaskName());
    assertNull(actualImageIOTask.getTaskType());
    assertNull(actualImageIOTask.getProject());
    assertNull(actualImageIOTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualImageIOTask.hasSelectors());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualImageIOTask, runtimeConfigurableWrapper.getProxy());
  }
}
