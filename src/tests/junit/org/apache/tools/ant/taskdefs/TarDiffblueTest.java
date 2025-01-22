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
import java.util.ArrayList;
import java.util.Collection;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Tar.TarCompressionMethod;
import org.apache.tools.ant.taskdefs.Tar.TarFileSet;
import org.apache.tools.ant.taskdefs.Tar.TarLongFileMode;
import org.apache.tools.ant.types.ArchiveFileSet;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.ZipFileSet;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class TarDiffblueTest {
  /**
   * Test {@link Tar#createTarFileSet()}.
   * <p>
   * Method under test: {@link Tar#createTarFileSet()}
   */
  @Test
  public void testCreateTarFileSet() {
    // Arrange
    Tar tar = new Tar();

    // Act
    TarFileSet actualCreateTarFileSetResult = tar.createTarFileSet();

    // Assert
    assertEquals("", actualCreateTarFileSetResult.getFullpath());
    assertEquals("", actualCreateTarFileSetResult.getPrefix());
    assertEquals("", actualCreateTarFileSetResult.getGroup());
    assertEquals("", actualCreateTarFileSetResult.getUserName());
    assertNull(actualCreateTarFileSetResult.getDir());
    assertNull(actualCreateTarFileSetResult.getSrc());
    assertNull(actualCreateTarFileSetResult.getDescription());
    assertNull(actualCreateTarFileSetResult.getEncoding());
    assertNull(actualCreateTarFileSetResult.getProject());
    assertNull(actualCreateTarFileSetResult.getRefid());
    assertEquals(0, actualCreateTarFileSetResult.getGid());
    assertEquals(0, actualCreateTarFileSetResult.getUid());
    assertEquals(1, tar.filesets.size());
    assertEquals(16877, actualCreateTarFileSetResult.getDirMode());
    assertEquals(33188, actualCreateTarFileSetResult.getMode());
    assertEquals(33188, actualCreateTarFileSetResult.getFileMode());
    assertEquals(5, actualCreateTarFileSetResult.getMaxLevelsOfSymlinks());
    assertFalse(actualCreateTarFileSetResult.getPreserveLeadingSlashes());
    assertFalse(actualCreateTarFileSetResult.isReference());
    assertFalse(actualCreateTarFileSetResult.hasGroupBeenSet());
    assertFalse(actualCreateTarFileSetResult.hasGroupIdBeenSet());
    assertFalse(actualCreateTarFileSetResult.hasUserIdBeenSet());
    assertFalse(actualCreateTarFileSetResult.hasUserNameBeenSet());
    assertTrue(actualCreateTarFileSetResult.getDefaultexcludes());
    assertTrue(actualCreateTarFileSetResult.getErrorOnMissingDir());
    assertTrue(actualCreateTarFileSetResult.isFilesystemOnly());
  }

  /**
   * Test {@link Tar#execute()}.
   * <p>
   * Method under test: {@link Tar#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> tar.execute());
  }

  /**
   * Test {@link Tar#execute()}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#execute()}
   */
  @Test
  public void testExecute_givenFileNameNameIsDot_thenThrowBuildException() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName(Manifest.ATTRIBUTE_NAME);

    FileName name2 = new FileName();
    name2.setName(".");

    FileList res = new FileList();
    res.setDir(Copy.NULL_FILE_PLACEHOLDER);
    res.addConfiguredFile(name2);
    res.addConfiguredFile(name);

    Tar tar = new Tar();
    tar.add(res);
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> tar.execute());
  }

  /**
   * Test {@link Tar#execute()}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Basedir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#execute()}
   */
  @Test
  public void testExecute_givenTarBasedirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Tar tar = new Tar();
    tar.setBasedir(Copy.NULL_FILE_PLACEHOLDER);
    tar.add(Path.systemBootClasspath);
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> tar.execute());
  }

  /**
   * Test {@link Tar#execute()}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Tarfile is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#execute()}
   */
  @Test
  public void testExecute_givenTarTarfileIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> tar.execute());
  }

  /**
   * Test {@link Tar#execute()}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#execute()}
   */
  @Test
  public void testExecute_givenTar_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Tar()).execute());
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{"."}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{".."}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Tar tar = new Tar();
    tar.setProject(project);
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{"."}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_givenTarProjectIsProject() {
    // Arrange
    Tar tar = new Tar();
    tar.setProject(new Project());
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{"."}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_givenTarProjectIsProject2() {
    // Arrange
    Tar tar = new Tar();
    tar.setProject(new Project());
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"Files", "/"}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..} and {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_whenArrayOfStringWithDotDotAndSlash() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"..", "/"}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_whenArrayOfStringWithDotDot_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{".."}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_whenArrayOfStringWithDot_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{"."}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_whenArrayOfStringWithEmptyString() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{""}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files} and {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_whenArrayOfStringWithFilesAndSlash() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"Files", "/"}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files} and {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_whenArrayOfStringWithFilesAndSlash2() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"Files", "/", "Files"}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_whenArrayOfStringWithFiles_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{"Files"}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>When array of {@link String} with {@code /}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_whenArrayOfStringWithSlash_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"/"}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[], File)} with {@code files}, {@code dir}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[], File)}
   */
  @Test
  public void testArchiveIsUpToDateWithFilesDir_whenEmptyArrayOfString_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{}, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Tar tar = new Tar();
    tar.setProject(project);
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"."}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_givenTarProjectIsProject() {
    // Arrange
    Tar tar = new Tar();
    tar.setProject(new Project());
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"Files", "/"}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_givenTarProjectIsProject_whenArrayOfStringWithDot() {
    // Arrange
    Tar tar = new Tar();
    tar.setProject(new Project());
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"."}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_whenArrayOfStringWithDotDot_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{".."}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_whenArrayOfStringWithDot_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"."}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_whenArrayOfStringWithEmptyString_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{""}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files} and {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_whenArrayOfStringWithFilesAndSlash() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"Files", "/"}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files} and {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_whenArrayOfStringWithFilesAndSlash2() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"Files", "/", "Files"}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_whenArrayOfStringWithFiles_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{"Files"}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>When array of {@link String} with {@code /}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_whenArrayOfStringWithSlash_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new String[]{"/"}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(String[])} with {@code files}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(String[])}
   */
  @Test
  public void testArchiveIsUpToDateWithFiles_whenEmptyArrayOfString_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new String[]{}));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new Resource("file attribute is null!", true, WaitFor.ONE_MILLISECOND)));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR2() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new Resource("file attribute is null!", true, Resource.UNKNOWN_SIZE)));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR3() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new Resource("file attribute is null!", true, Long.MAX_VALUE)));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR_givenFileAttributeIsNull() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(r));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new Resource()));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR_thenReturnTrue2() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(tar.archiveIsUpToDate(new Resource("file attribute is null!", true, WaitFor.ONE_MILLISECOND)));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR_whenJavaConstantResource_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new JavaConstantResource()));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR_whenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);
    Resource r = new Resource();

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new MappedResource(r, new CutDirsMapper())));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with name is {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR_whenResourceWithNameIsFileAttributeIsNull() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new Resource("file attribute is null!")));
  }

  /**
   * Test {@link Tar#archiveIsUpToDate(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#archiveIsUpToDate(Resource)}
   */
  @Test
  public void testArchiveIsUpToDateWithR_whenResource_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.archiveIsUpToDate(new Resource()));
  }

  /**
   * Test {@link Tar#supportsNonFileResources()}.
   * <p>
   * Method under test: {@link Tar#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources() {
    // Arrange, Act and Assert
    assertTrue((new Tar()).supportsNonFileResources());
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    ArrayList<String> files = new ArrayList<>();
    files.add("..");

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code ..}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenDotDot_whenArrayListAddDotDot_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add("..");

    // Act and Assert
    assertFalse(tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenDot_whenArrayListAddDot_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add(".");

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenEmptyString_thenThrowBuildException() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add("");

    // Act and Assert
    assertThrows(BuildException.class, () -> tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenEmptyString_thenThrowBuildException2() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add("");
    files.add(".");

    // Act and Assert
    assertThrows(BuildException.class, () -> tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given {@code No sources found.}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code No sources found.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenNoSourcesFound_whenArrayListAddNoSourcesFound() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add("No sources found.");

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given {@code No sources found.}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code No sources found.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenNoSourcesFound_whenArrayListAddNoSourcesFound2() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add("No sources found.");
    files.add("No sources found.");

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given {@code NULL_FILE}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenNullFile_whenArrayListAddNullFile() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add("NULL_FILE");
    files.add("No sources found.");

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given {@code NULL_FILE}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenNullFile_whenArrayListAddNullFile2() {
    // Arrange
    Tar tar = new Tar();
    tar.setProject(new Project());
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add("NULL_FILE");
    files.add("No sources found.");

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Tar tar = new Tar();
    tar.setProject(project);
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add("No sources found.");

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_givenTarProjectIsProject() {
    // Arrange
    Tar tar = new Tar();
    tar.setProject(new Project());
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    ArrayList<String> files = new ArrayList<>();
    files.add("No sources found.");

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, files));
  }

  /**
   * Test {@link Tar#check(File, Collection)} with {@code File}, {@code Collection}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, Collection)}
   */
  @Test
  public void testCheckWithFileCollection_whenArrayList_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, new ArrayList<>()));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{".."}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Tar tar = new Tar();
    tar.setProject(project);
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{"."}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_givenTarProjectIsProject() {
    // Arrange
    Tar tar = new Tar();
    tar.setProject(new Project());
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files", "/"}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_givenTarProjectIsProject_whenArrayOfStringWithDot() {
    // Arrange
    Tar tar = new Tar();
    tar.setProject(new Project());
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{"."}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..} and {@code /}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_whenArrayOfStringWithDotDotAndSlash_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{"..", "/"}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_whenArrayOfStringWithDotDot_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{".."}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_whenArrayOfStringWithDot_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{"."}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_whenArrayOfStringWithEmptyString_thenThrowBuildException() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{""}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files} and {@code /}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_whenArrayOfStringWithFilesAndSlash_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files", "/"}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files} and {@code /}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_whenArrayOfStringWithFilesAndSlash_thenReturnFalse2() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files", "/", "Files"}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>When array of {@link String} with {@code Files}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_whenArrayOfStringWithFiles_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>When array of {@link String} with {@code /}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_whenArrayOfStringWithSlash_thenReturnFalse() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{"/"}));
  }

  /**
   * Test {@link Tar#check(File, String[])} with {@code File}, {@code String[]}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(File, String[])}
   */
  @Test
  public void testCheckWithFileString_whenEmptyArrayOfString_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();
    tar.setTarfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(tar.check(Copy.NULL_FILE_PLACEHOLDER, new String[]{}));
  }

  /**
   * Test {@link Tar#check(ResourceCollection)} with {@code ResourceCollection}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor).</li>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(ResourceCollection)}
   */
  @Test
  public void testCheckWithResourceCollection_givenTar_whenFileList_thenReturnTrue() {
    // Arrange
    Tar tar = new Tar();

    // Act and Assert
    assertTrue(tar.check(new FileList()));
  }

  /**
   * Test {@link Tar#check(ResourceCollection)} with {@code ResourceCollection}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#check(ResourceCollection)}
   */
  @Test
  public void testCheckWithResourceCollection_givenTar_whenNone_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Tar()).check(Resources.NONE));
  }

  /**
   * Test {@link Tar#isFileFileSet(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#isFileFileSet(ResourceCollection)}
   */
  @Test
  public void testIsFileFileSet_givenResource_thenReturnFalse() {
    // Arrange
    org.apache.tools.ant.types.TarFileSet rc = new org.apache.tools.ant.types.TarFileSet();
    rc.setSrcResource(new Resource());
    rc.appendSelector(new ScriptSelector());

    // Act and Assert
    assertFalse(Tar.isFileFileSet(rc));
  }

  /**
   * Test {@link Tar#isFileFileSet(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link ScriptSelector} (default constructor).</li>
   *   <li>When {@link org.apache.tools.ant.types.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#isFileFileSet(ResourceCollection)}
   */
  @Test
  public void testIsFileFileSet_givenScriptSelector_whenTarFileSetAppendSelectorScriptSelector() {
    // Arrange
    org.apache.tools.ant.types.TarFileSet rc = new org.apache.tools.ant.types.TarFileSet();
    rc.appendSelector(new ScriptSelector());

    // Act and Assert
    assertTrue(Tar.isFileFileSet(rc));
  }

  /**
   * Test {@link Tar#isFileFileSet(ResourceCollection)}.
   * <ul>
   *   <li>When {@link FileSet#FileSet()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#isFileFileSet(ResourceCollection)}
   */
  @Test
  public void testIsFileFileSet_whenFileSet_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Tar.isFileFileSet(new FileSet()));
  }

  /**
   * Test {@link Tar#isFileFileSet(ResourceCollection)}.
   * <ul>
   *   <li>When {@link org.apache.tools.ant.types.TarFileSet#TarFileSet()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#isFileFileSet(ResourceCollection)}
   */
  @Test
  public void testIsFileFileSet_whenTarFileSet_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Tar.isFileFileSet(new org.apache.tools.ant.types.TarFileSet()));
  }

  /**
   * Test {@link Tar#asTarFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@code Prefix}.</li>
   *   <li>When {@link org.apache.tools.ant.types.TarFileSet#TarFileSet()} Prefix is {@code Prefix}.</li>
   *   <li>Then return {@code Prefix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#asTarFileSet(ArchiveFileSet)}
   */
  @Test
  public void testAsTarFileSet_givenPrefix_whenTarFileSetPrefixIsPrefix_thenReturnPrefix() {
    // Arrange
    Tar tar = new Tar();

    org.apache.tools.ant.types.TarFileSet archiveFileSet = new org.apache.tools.ant.types.TarFileSet();
    archiveFileSet.setPrefix("Prefix");
    archiveFileSet.appendSelector(new ScriptSelector());

    // Act
    TarFileSet actualAsTarFileSetResult = tar.asTarFileSet(archiveFileSet);

    // Assert
    assertEquals("", actualAsTarFileSetResult.getFullpath());
    assertEquals("", actualAsTarFileSetResult.getGroup());
    assertEquals("", actualAsTarFileSetResult.getUserName());
    assertEquals("Prefix", actualAsTarFileSetResult.getPrefix());
    assertNull(actualAsTarFileSetResult.getDir());
    assertNull(actualAsTarFileSetResult.getSrc());
    assertNull(actualAsTarFileSetResult.getDescription());
    assertNull(actualAsTarFileSetResult.getEncoding());
    assertNull(actualAsTarFileSetResult.getProject());
    assertNull(actualAsTarFileSetResult.getRefid());
    assertEquals(0, actualAsTarFileSetResult.getGid());
    assertEquals(0, actualAsTarFileSetResult.getUid());
    assertEquals(16877, actualAsTarFileSetResult.getDirMode());
    assertEquals(33188, actualAsTarFileSetResult.getMode());
    assertEquals(33188, actualAsTarFileSetResult.getFileMode());
    assertEquals(5, actualAsTarFileSetResult.getMaxLevelsOfSymlinks());
    assertFalse(actualAsTarFileSetResult.getPreserveLeadingSlashes());
    assertFalse(actualAsTarFileSetResult.isReference());
    assertFalse(actualAsTarFileSetResult.hasGroupBeenSet());
    assertFalse(actualAsTarFileSetResult.hasGroupIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserNameBeenSet());
    assertTrue(actualAsTarFileSetResult.getDefaultexcludes());
    assertTrue(actualAsTarFileSetResult.getErrorOnMissingDir());
    assertTrue(actualAsTarFileSetResult.isFilesystemOnly());
  }

  /**
   * Test {@link Tar#asTarFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@link ScriptSelector} (default constructor).</li>
   *   <li>When {@link org.apache.tools.ant.types.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#asTarFileSet(ArchiveFileSet)}
   */
  @Test
  public void testAsTarFileSet_givenScriptSelector_whenTarFileSetAppendSelectorScriptSelector() {
    // Arrange
    Tar tar = new Tar();

    org.apache.tools.ant.types.TarFileSet archiveFileSet = new org.apache.tools.ant.types.TarFileSet();
    archiveFileSet.appendSelector(new ScriptSelector());

    // Act
    TarFileSet actualAsTarFileSetResult = tar.asTarFileSet(archiveFileSet);

    // Assert
    assertEquals("", actualAsTarFileSetResult.getFullpath());
    assertEquals("", actualAsTarFileSetResult.getPrefix());
    assertEquals("", actualAsTarFileSetResult.getGroup());
    assertEquals("", actualAsTarFileSetResult.getUserName());
    assertNull(actualAsTarFileSetResult.getDir());
    assertNull(actualAsTarFileSetResult.getSrc());
    assertNull(actualAsTarFileSetResult.getDescription());
    assertNull(actualAsTarFileSetResult.getEncoding());
    assertNull(actualAsTarFileSetResult.getProject());
    assertNull(actualAsTarFileSetResult.getRefid());
    assertEquals(0, actualAsTarFileSetResult.getGid());
    assertEquals(0, actualAsTarFileSetResult.getUid());
    assertEquals(16877, actualAsTarFileSetResult.getDirMode());
    assertEquals(33188, actualAsTarFileSetResult.getMode());
    assertEquals(33188, actualAsTarFileSetResult.getFileMode());
    assertEquals(5, actualAsTarFileSetResult.getMaxLevelsOfSymlinks());
    assertFalse(actualAsTarFileSetResult.getPreserveLeadingSlashes());
    assertFalse(actualAsTarFileSetResult.isReference());
    assertFalse(actualAsTarFileSetResult.hasGroupBeenSet());
    assertFalse(actualAsTarFileSetResult.hasGroupIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserNameBeenSet());
    assertTrue(actualAsTarFileSetResult.getDefaultexcludes());
    assertTrue(actualAsTarFileSetResult.getErrorOnMissingDir());
    assertTrue(actualAsTarFileSetResult.isFilesystemOnly());
  }

  /**
   * Test {@link Tar#asTarFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#asTarFileSet(ArchiveFileSet)}
   */
  @Test
  public void testAsTarFileSet_givenTarProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Tar tar = new Tar();
    Project project = new Project();
    tar.setProject(project);

    // Act and Assert
    assertSame(project, tar.asTarFileSet(new org.apache.tools.ant.types.TarFileSet()).getProject());
  }

  /**
   * Test {@link Tar#asTarFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return Prefix is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#asTarFileSet(ArchiveFileSet)}
   */
  @Test
  public void testAsTarFileSet_givenTar_whenNull_thenReturnPrefixIsEmptyString() {
    // Arrange and Act
    TarFileSet actualAsTarFileSetResult = (new Tar()).asTarFileSet(null);

    // Assert
    assertEquals("", actualAsTarFileSetResult.getFullpath());
    assertEquals("", actualAsTarFileSetResult.getPrefix());
    assertEquals("", actualAsTarFileSetResult.getGroup());
    assertEquals("", actualAsTarFileSetResult.getUserName());
    assertNull(actualAsTarFileSetResult.getDir());
    assertNull(actualAsTarFileSetResult.getSrc());
    assertNull(actualAsTarFileSetResult.getDescription());
    assertNull(actualAsTarFileSetResult.getEncoding());
    assertNull(actualAsTarFileSetResult.getProject());
    assertNull(actualAsTarFileSetResult.getRefid());
    assertEquals(0, actualAsTarFileSetResult.getGid());
    assertEquals(0, actualAsTarFileSetResult.getUid());
    assertEquals(16877, actualAsTarFileSetResult.getDirMode());
    assertEquals(33188, actualAsTarFileSetResult.getMode());
    assertEquals(33188, actualAsTarFileSetResult.getFileMode());
    assertEquals(5, actualAsTarFileSetResult.getMaxLevelsOfSymlinks());
    assertFalse(actualAsTarFileSetResult.getPreserveLeadingSlashes());
    assertFalse(actualAsTarFileSetResult.isReference());
    assertFalse(actualAsTarFileSetResult.hasGroupBeenSet());
    assertFalse(actualAsTarFileSetResult.hasGroupIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserNameBeenSet());
    assertTrue(actualAsTarFileSetResult.getDefaultexcludes());
    assertTrue(actualAsTarFileSetResult.getErrorOnMissingDir());
    assertTrue(actualAsTarFileSetResult.isFilesystemOnly());
  }

  /**
   * Test {@link Tar#asTarFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor).</li>
   *   <li>When {@link org.apache.tools.ant.types.TarFileSet#TarFileSet()}.</li>
   *   <li>Then return Prefix is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#asTarFileSet(ArchiveFileSet)}
   */
  @Test
  public void testAsTarFileSet_givenTar_whenTarFileSet_thenReturnPrefixIsEmptyString() {
    // Arrange
    Tar tar = new Tar();

    // Act
    TarFileSet actualAsTarFileSetResult = tar.asTarFileSet(new org.apache.tools.ant.types.TarFileSet());

    // Assert
    assertEquals("", actualAsTarFileSetResult.getFullpath());
    assertEquals("", actualAsTarFileSetResult.getPrefix());
    assertEquals("", actualAsTarFileSetResult.getGroup());
    assertEquals("", actualAsTarFileSetResult.getUserName());
    assertNull(actualAsTarFileSetResult.getDir());
    assertNull(actualAsTarFileSetResult.getSrc());
    assertNull(actualAsTarFileSetResult.getDescription());
    assertNull(actualAsTarFileSetResult.getEncoding());
    assertNull(actualAsTarFileSetResult.getProject());
    assertNull(actualAsTarFileSetResult.getRefid());
    assertEquals(0, actualAsTarFileSetResult.getGid());
    assertEquals(0, actualAsTarFileSetResult.getUid());
    assertEquals(16877, actualAsTarFileSetResult.getDirMode());
    assertEquals(33188, actualAsTarFileSetResult.getMode());
    assertEquals(33188, actualAsTarFileSetResult.getFileMode());
    assertEquals(5, actualAsTarFileSetResult.getMaxLevelsOfSymlinks());
    assertFalse(actualAsTarFileSetResult.getPreserveLeadingSlashes());
    assertFalse(actualAsTarFileSetResult.isReference());
    assertFalse(actualAsTarFileSetResult.hasGroupBeenSet());
    assertFalse(actualAsTarFileSetResult.hasGroupIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserNameBeenSet());
    assertTrue(actualAsTarFileSetResult.getDefaultexcludes());
    assertTrue(actualAsTarFileSetResult.getErrorOnMissingDir());
    assertTrue(actualAsTarFileSetResult.isFilesystemOnly());
  }

  /**
   * Test {@link Tar#asTarFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor).</li>
   *   <li>When {@link TarFileSet#TarFileSet()}.</li>
   *   <li>Then return {@link TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#asTarFileSet(ArchiveFileSet)}
   */
  @Test
  public void testAsTarFileSet_givenTar_whenTarFileSet_thenReturnTarFileSet() {
    // Arrange
    Tar tar = new Tar();
    TarFileSet archiveFileSet = new TarFileSet();

    // Act and Assert
    assertSame(archiveFileSet, tar.asTarFileSet(archiveFileSet));
  }

  /**
   * Test {@link Tar#asTarFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@link Tar} (default constructor).</li>
   *   <li>When {@link ZipFileSet#ZipFileSet()}.</li>
   *   <li>Then return Prefix is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tar#asTarFileSet(ArchiveFileSet)}
   */
  @Test
  public void testAsTarFileSet_givenTar_whenZipFileSet_thenReturnPrefixIsEmptyString() {
    // Arrange
    Tar tar = new Tar();

    // Act
    TarFileSet actualAsTarFileSetResult = tar.asTarFileSet(new ZipFileSet());

    // Assert
    assertEquals("", actualAsTarFileSetResult.getFullpath());
    assertEquals("", actualAsTarFileSetResult.getPrefix());
    assertEquals("", actualAsTarFileSetResult.getGroup());
    assertEquals("", actualAsTarFileSetResult.getUserName());
    assertNull(actualAsTarFileSetResult.getDir());
    assertNull(actualAsTarFileSetResult.getSrc());
    assertNull(actualAsTarFileSetResult.getDescription());
    assertNull(actualAsTarFileSetResult.getEncoding());
    assertNull(actualAsTarFileSetResult.getProject());
    assertNull(actualAsTarFileSetResult.getRefid());
    assertEquals(0, actualAsTarFileSetResult.getGid());
    assertEquals(0, actualAsTarFileSetResult.getUid());
    assertEquals(16877, actualAsTarFileSetResult.getDirMode());
    assertEquals(33188, actualAsTarFileSetResult.getMode());
    assertEquals(33188, actualAsTarFileSetResult.getFileMode());
    assertEquals(5, actualAsTarFileSetResult.getMaxLevelsOfSymlinks());
    assertFalse(actualAsTarFileSetResult.getPreserveLeadingSlashes());
    assertFalse(actualAsTarFileSetResult.isReference());
    assertFalse(actualAsTarFileSetResult.hasGroupBeenSet());
    assertFalse(actualAsTarFileSetResult.hasGroupIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserIdBeenSet());
    assertFalse(actualAsTarFileSetResult.hasUserNameBeenSet());
    assertTrue(actualAsTarFileSetResult.getDefaultexcludes());
    assertTrue(actualAsTarFileSetResult.getErrorOnMissingDir());
    assertTrue(actualAsTarFileSetResult.isFilesystemOnly());
  }

  /**
   * Test new {@link Tar} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Tar}
   */
  @Test
  public void testNewTar() {
    // Arrange and Act
    Tar actualTar = new Tar();

    // Assert
    assertNull(actualTar.baseDir);
    assertNull(actualTar.tarFile);
    assertNull(actualTar.getDescription());
    assertNull(actualTar.getTaskName());
    assertNull(actualTar.getTaskType());
    assertNull(actualTar.getProject());
    assertNull(actualTar.getOwningTarget());
    assertFalse(actualTar.hasSelectors());
    assertTrue(actualTar.filesets.isEmpty());
  }

  /**
   * Test TarCompressionMethod {@link TarCompressionMethod#getValues()}.
   * <p>
   * Method under test: {@link TarCompressionMethod#getValues()}
   */
  @Test
  public void testTarCompressionMethodGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"none", "gzip", "bzip2", "xz"}, (new TarCompressionMethod()).getValues());
  }

  /**
   * Test TarCompressionMethod new {@link TarCompressionMethod} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TarCompressionMethod}
   */
  @Test
  public void testTarCompressionMethodNewTarCompressionMethod() {
    // Arrange and Act
    TarCompressionMethod actualTarCompressionMethod = new TarCompressionMethod();

    // Assert
    assertEquals("none", actualTarCompressionMethod.getValue());
    assertEquals(0, actualTarCompressionMethod.getIndex());
    assertArrayEquals(new String[]{"none", "gzip", "bzip2", "xz"}, actualTarCompressionMethod.getValues());
  }

  /**
   * Test TarFileSet {@link TarFileSet#getMode()}.
   * <p>
   * Method under test: {@link TarFileSet#getMode()}
   */
  @Test
  public void testTarFileSetGetMode() {
    // Arrange, Act and Assert
    assertEquals(33188, (new TarFileSet()).getMode());
  }

  /**
   * Test TarFileSet getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TarFileSet#setPreserveLeadingSlashes(boolean)}
   *   <li>{@link TarFileSet#getPreserveLeadingSlashes()}
   * </ul>
   */
  @Test
  public void testTarFileSetGettersAndSetters() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setPreserveLeadingSlashes(true);

    // Assert
    assertTrue(tarFileSet.getPreserveLeadingSlashes());
  }

  /**
   * Test TarFileSet {@link TarFileSet#TarFileSet()}.
   * <p>
   * Method under test: {@link TarFileSet#TarFileSet()}
   */
  @Test
  public void testTarFileSetNewTarFileSet() {
    // Arrange and Act
    TarFileSet actualTarFileSet = new TarFileSet();

    // Assert
    assertEquals("", actualTarFileSet.getFullpath());
    assertEquals("", actualTarFileSet.getPrefix());
    assertEquals("", actualTarFileSet.getGroup());
    assertEquals("", actualTarFileSet.getUserName());
    assertNull(actualTarFileSet.getDir());
    assertNull(actualTarFileSet.getSrc());
    assertNull(actualTarFileSet.getDescription());
    assertNull(actualTarFileSet.getEncoding());
    assertNull(actualTarFileSet.getProject());
    assertNull(actualTarFileSet.getRefid());
    assertEquals(0, actualTarFileSet.getGid());
    assertEquals(0, actualTarFileSet.getUid());
    assertEquals(16877, actualTarFileSet.getDirMode());
    assertEquals(33188, actualTarFileSet.getMode());
    assertEquals(33188, actualTarFileSet.getFileMode());
    assertEquals(5, actualTarFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualTarFileSet.getPreserveLeadingSlashes());
    assertFalse(actualTarFileSet.isReference());
    assertFalse(actualTarFileSet.hasGroupBeenSet());
    assertFalse(actualTarFileSet.hasGroupIdBeenSet());
    assertFalse(actualTarFileSet.hasUserIdBeenSet());
    assertFalse(actualTarFileSet.hasUserNameBeenSet());
    assertTrue(actualTarFileSet.getDefaultexcludes());
    assertTrue(actualTarFileSet.getErrorOnMissingDir());
    assertTrue(actualTarFileSet.isFilesystemOnly());
  }

  /**
   * Test TarFileSet {@link TarFileSet#TarFileSet(FileSet)}.
   * <ul>
   *   <li>When {@link FileSet#FileSet()}.</li>
   *   <li>Then return Fullpath is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#TarFileSet(FileSet)}
   */
  @Test
  public void testTarFileSetNewTarFileSet_whenFileSet_thenReturnFullpathIsEmptyString() {
    // Arrange and Act
    TarFileSet actualTarFileSet = new TarFileSet(new FileSet());

    // Assert
    assertEquals("", actualTarFileSet.getFullpath());
    assertEquals("", actualTarFileSet.getPrefix());
    assertEquals("", actualTarFileSet.getGroup());
    assertEquals("", actualTarFileSet.getUserName());
    assertNull(actualTarFileSet.getDir());
    assertNull(actualTarFileSet.getSrc());
    assertNull(actualTarFileSet.getDescription());
    assertNull(actualTarFileSet.getEncoding());
    assertNull(actualTarFileSet.getProject());
    assertNull(actualTarFileSet.getRefid());
    assertEquals(0, actualTarFileSet.getGid());
    assertEquals(0, actualTarFileSet.getUid());
    assertEquals(16877, actualTarFileSet.getDirMode());
    assertEquals(33188, actualTarFileSet.getMode());
    assertEquals(33188, actualTarFileSet.getFileMode());
    assertEquals(5, actualTarFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualTarFileSet.getPreserveLeadingSlashes());
    assertFalse(actualTarFileSet.isReference());
    assertFalse(actualTarFileSet.hasGroupBeenSet());
    assertFalse(actualTarFileSet.hasGroupIdBeenSet());
    assertFalse(actualTarFileSet.hasUserIdBeenSet());
    assertFalse(actualTarFileSet.hasUserNameBeenSet());
    assertTrue(actualTarFileSet.getDefaultexcludes());
    assertTrue(actualTarFileSet.getErrorOnMissingDir());
    assertTrue(actualTarFileSet.isFilesystemOnly());
  }

  /**
   * Test TarFileSet {@link TarFileSet#setMode(String)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link TarFileSet#TarFileSet()} Mode is {@code 32802}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setMode(String)}
   */
  @Test
  public void testTarFileSetSetMode_givenTarFileSet_when42_thenTarFileSetModeIs32802() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setMode("42");

    // Assert
    assertEquals(32802, tarFileSet.getMode());
    assertEquals(32802, tarFileSet.getFileMode());
  }

  /**
   * Test TarLongFileMode {@link TarLongFileMode#getValues()}.
   * <p>
   * Method under test: {@link TarLongFileMode#getValues()}
   */
  @Test
  public void testTarLongFileModeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{TarLongFileMode.WARN, TarLongFileMode.FAIL, TarLongFileMode.TRUNCATE,
        TarLongFileMode.GNU, TarLongFileMode.POSIX, TarLongFileMode.OMIT}, (new TarLongFileMode()).getValues());
  }

  /**
   * Test TarLongFileMode {@link TarLongFileMode#isFailMode()}.
   * <p>
   * Method under test: {@link TarLongFileMode#isFailMode()}
   */
  @Test
  public void testTarLongFileModeIsFailMode() {
    // Arrange, Act and Assert
    assertFalse((new TarLongFileMode()).isFailMode());
  }

  /**
   * Test TarLongFileMode {@link TarLongFileMode#isGnuMode()}.
   * <p>
   * Method under test: {@link TarLongFileMode#isGnuMode()}
   */
  @Test
  public void testTarLongFileModeIsGnuMode() {
    // Arrange, Act and Assert
    assertFalse((new TarLongFileMode()).isGnuMode());
  }

  /**
   * Test TarLongFileMode {@link TarLongFileMode#isOmitMode()}.
   * <p>
   * Method under test: {@link TarLongFileMode#isOmitMode()}
   */
  @Test
  public void testTarLongFileModeIsOmitMode() {
    // Arrange, Act and Assert
    assertFalse((new TarLongFileMode()).isOmitMode());
  }

  /**
   * Test TarLongFileMode {@link TarLongFileMode#isPosixMode()}.
   * <p>
   * Method under test: {@link TarLongFileMode#isPosixMode()}
   */
  @Test
  public void testTarLongFileModeIsPosixMode() {
    // Arrange, Act and Assert
    assertFalse((new TarLongFileMode()).isPosixMode());
  }

  /**
   * Test TarLongFileMode {@link TarLongFileMode#isTruncateMode()}.
   * <p>
   * Method under test: {@link TarLongFileMode#isTruncateMode()}
   */
  @Test
  public void testTarLongFileModeIsTruncateMode() {
    // Arrange, Act and Assert
    assertFalse((new TarLongFileMode()).isTruncateMode());
  }

  /**
   * Test TarLongFileMode {@link TarLongFileMode#isWarnMode()}.
   * <p>
   * Method under test: {@link TarLongFileMode#isWarnMode()}
   */
  @Test
  public void testTarLongFileModeIsWarnMode() {
    // Arrange, Act and Assert
    assertTrue((new TarLongFileMode()).isWarnMode());
  }

  /**
   * Test TarLongFileMode new {@link TarLongFileMode} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TarLongFileMode}
   */
  @Test
  public void testTarLongFileModeNewTarLongFileMode() {
    // Arrange and Act
    TarLongFileMode actualTarLongFileMode = new TarLongFileMode();

    // Assert
    assertEquals(0, actualTarLongFileMode.getIndex());
    assertEquals(TarLongFileMode.WARN, actualTarLongFileMode.getValue());
    assertArrayEquals(new String[]{TarLongFileMode.WARN, TarLongFileMode.FAIL, TarLongFileMode.TRUNCATE,
        TarLongFileMode.GNU, TarLongFileMode.POSIX, TarLongFileMode.OMIT}, actualTarLongFileMode.getValues());
  }
}
