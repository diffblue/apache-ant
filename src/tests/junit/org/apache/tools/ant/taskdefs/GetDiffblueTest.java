package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Get.DownloadProgress;
import org.apache.tools.ant.taskdefs.Get.NullProgress;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.PolyTest;
import org.apache.tools.ant.types.PolyTest.MyPath;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.Resources;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class GetDiffblueTest {
  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenConcatAddFilelistFileList_thenThrowBuildException() throws BuildException {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    Get get = new Get();
    get.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code Text}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenConcatAddTextText_thenThrowBuildException() throws BuildException {
    // Arrange
    Concat rc = new Concat();
    rc.addText("Text");

    Get get = new Get();
    get.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenConcatDestIsResource_thenThrowBuildException() throws BuildException {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    Get get = new Get();
    get.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenConcatProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Concat rc = new Concat();
    rc.setProject(new Project());
    rc.addText("Text");

    Get get = new Get();
    get.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link MyPath#MyPath(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenGetAddMyPathWithProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Get get = new Get();
    get.add(new MyPath(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenGetAddNone_thenThrowBuildException() throws BuildException {
    // Arrange
    Get get = new Get();
    get.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code No directory specified for %s.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenGetAddPathWithPIsProjectAndPathIsNoDirectorySpecifiedForS() throws BuildException {
    // Arrange
    Get get = new Get();
    get.add(new Path(new Project(), "No directory specified for %s."));

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenGetAddPathWithProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Get get = new Get();
    get.add(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenGetAddResource_thenThrowBuildException() throws BuildException {
    // Arrange
    Get get = new Get();
    get.add(new Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Get} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenGet_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Get()).execute());
  }

  /**
   * Test {@link Get#execute()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#execute()}
   */
  @Test
  public void testExecute_givenPathWithProjectIsProjectAddFilelistFileList() throws BuildException {
    // Arrange
    Path rc = new Path(new Project());
    rc.addFilelist(new FileList());

    Get get = new Get();
    get.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.execute());
  }

  /**
   * Test {@link Get#doGet(int, DownloadProgress)} with {@code logLevel}, {@code progress}.
   * <p>
   * Method under test: {@link Get#doGet(int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithLogLevelProgress() throws IOException {
    // Arrange
    Get get = new Get();
    get.add(new Path(new Project(), "at least one source is required"));

    // Act and Assert
    assertThrows(BuildException.class, () -> get.doGet(1, new NullProgress()));
  }

  /**
   * Test {@link Get#doGet(int, DownloadProgress)} with {@code logLevel}, {@code progress}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#doGet(int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithLogLevelProgress_givenConcatAddFilelistFileList() throws IOException {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    Get get = new Get();
    get.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.doGet(1, new NullProgress()));
  }

  /**
   * Test {@link Get#doGet(int, DownloadProgress)} with {@code logLevel}, {@code progress}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code cr}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#doGet(int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithLogLevelProgress_givenConcatAddTextCr_thenThrowBuildException() throws IOException {
    // Arrange
    Concat rc = new Concat();
    rc.addText("cr");

    Get get = new Get();
    get.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.doGet(1, new NullProgress()));
  }

  /**
   * Test {@link Get#doGet(int, DownloadProgress)} with {@code logLevel}, {@code progress}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#doGet(int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithLogLevelProgress_givenConcatDestIsResource_thenThrowBuildException() throws IOException {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    Get get = new Get();
    get.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.doGet(1, new NullProgress()));
  }

  /**
   * Test {@link Get#doGet(int, DownloadProgress)} with {@code logLevel}, {@code progress}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#doGet(int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithLogLevelProgress_givenConcatProjectIsProject() throws IOException {
    // Arrange
    Concat rc = new Concat();
    rc.setProject(new Project());
    rc.addText("cr");

    Get get = new Get();
    get.add(rc);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.doGet(1, new NullProgress()));
  }

  /**
   * Test {@link Get#doGet(int, DownloadProgress)} with {@code logLevel}, {@code progress}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link MyPath#MyPath(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#doGet(int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithLogLevelProgress_givenGetAddMyPathWithProjectIsProject() throws IOException {
    // Arrange
    Get get = new Get();
    get.add(new MyPath(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> get.doGet(1, new NullProgress()));
  }

  /**
   * Test {@link Get#doGet(int, DownloadProgress)} with {@code logLevel}, {@code progress}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#doGet(int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithLogLevelProgress_givenGetAddNone_thenThrowBuildException() throws IOException {
    // Arrange
    Get get = new Get();
    get.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> get.doGet(1, new NullProgress()));
  }

  /**
   * Test {@link Get#doGet(int, DownloadProgress)} with {@code logLevel}, {@code progress}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#doGet(int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithLogLevelProgress_givenGetAddPathWithProjectIsProject() throws IOException {
    // Arrange
    Get get = new Get();
    get.add(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> get.doGet(1, new NullProgress()));
  }

  /**
   * Test {@link Get#doGet(int, DownloadProgress)} with {@code logLevel}, {@code progress}.
   * <ul>
   *   <li>Given {@link Get} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#doGet(int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithLogLevelProgress_givenGet_thenThrowBuildException() throws IOException {
    // Arrange
    Get get = new Get();

    // Act and Assert
    assertThrows(BuildException.class, () -> get.doGet(1, new NullProgress()));
  }

  /**
   * Test {@link Get#doGet(URL, File, int, DownloadProgress)} with {@code source}, {@code dest}, {@code logLevel}, {@code progress}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#doGet(URL, File, int, DownloadProgress)}
   */
  @Test
  public void testDoGetWithSourceDestLogLevelProgress_thenReturnTrue() throws IOException {
    // Arrange
    Get get = new Get();
    get.setSkipExisting(true);
    URL source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL();
    File dest = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(get.doGet(source, dest, 1, new NullProgress()));
  }

  /**
   * Test {@link Get#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_givenGetAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    Get get = new Get();
    get.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> get.add(new CutDirsMapper()));
  }

  /**
   * Test {@link Get#isMoved(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#isMoved(int)}
   */
  @Test
  public void testIsMoved_whenOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Get.isMoved(1));
  }

  /**
   * Test {@link Get#isMoved(int)}.
   * <ul>
   *   <li>When three hundred one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#isMoved(int)}
   */
  @Test
  public void testIsMoved_whenThreeHundredOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Get.isMoved(301));
  }

  /**
   * Test {@link Get#isMoved(int)}.
   * <ul>
   *   <li>When three hundred seven.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#isMoved(int)}
   */
  @Test
  public void testIsMoved_whenThreeHundredSeven_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Get.isMoved(307));
  }

  /**
   * Test {@link Get#isMoved(int)}.
   * <ul>
   *   <li>When three hundred three.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#isMoved(int)}
   */
  @Test
  public void testIsMoved_whenThreeHundredThree_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Get.isMoved(303));
  }

  /**
   * Test {@link Get#isMoved(int)}.
   * <ul>
   *   <li>When three hundred two.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#isMoved(int)}
   */
  @Test
  public void testIsMoved_whenThreeHundredTwo_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Get.isMoved(302));
  }

  /**
   * Test {@link Get#createMapper()}.
   * <ul>
   *   <li>Given {@link Get} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#createMapper()}
   */
  @Test
  public void testCreateMapper_givenGetAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    Get get = new Get();
    get.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> get.createMapper());
  }

  /**
   * Test {@link Get#createMapper()}.
   * <ul>
   *   <li>Given {@link Get} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Get#createMapper()}
   */
  @Test
  public void testCreateMapper_givenGet_thenReturnLocationFileNameIsNull() throws BuildException {
    // Arrange and Act
    Mapper actualCreateMapperResult = (new Get()).createMapper();

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
   * Test new {@link Get} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Get}
   */
  @Test
  public void testNewGet() {
    // Arrange and Act
    Get actualGet = new Get();

    // Assert
    Location location = actualGet.getLocation();
    assertNull(location.getFileName());
    assertNull(actualGet.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualGet.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualGet.getTaskName());
    assertNull(actualGet.getTaskType());
    assertNull(actualGet.getProject());
    assertNull(actualGet.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualGet, runtimeConfigurableWrapper.getProxy());
  }
}
