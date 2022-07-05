package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.ant.antunit.AntUnit;
import org.apache.ant.antunit.LogContent;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.input.DefaultInputHandler;
import org.apache.tools.ant.listener.CommonsLoggingListener;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.LogLevel;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.ResourceFactory;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.optional.ScriptMapper;
import org.apache.tools.ant.types.optional.xz.XzResource;
import org.apache.tools.ant.types.resources.FileProvider;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.Touchable;
import org.apache.tools.ant.types.resources.Union;
import org.apache.tools.ant.util.facade.ImplementationSpecificArgument;
import org.junit.Test;

public class ResourceUtilsDiffblueTest {
  /**
  * Method under test: {@link ResourceUtils#asFileResource(FileProvider)}
  */
  @Test
  public void testAsFileResource() {
    // Arrange, Act and Assert
    assertNull(ResourceUtils.asFileResource(null));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent() throws IOException {
    // Arrange
    Resource r1 = new Resource("Name");

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent2() throws IOException {
    // Arrange
    Resource r1 = new Resource("org.apache.tools.ant.types.Resource");

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent3() throws IOException {
    // Arrange
    Resource r1 = new Resource();

    // Act and Assert
    assertEquals(1, ResourceUtils.compareContent(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent4() throws IOException {
    // Arrange
    JavaConstantResource r1 = new JavaConstantResource();

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent5() throws IOException {
    // Arrange
    Resource r1 = new Resource("Name");

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, ResourceUtils.compareContent(r1, new Resource(), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent6() throws IOException {
    // Arrange
    Resource r1 = new Resource("org.apache.tools.ant.types.Resource");

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER,
        ResourceUtils.compareContent(r1, new Resource("Name", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent7() throws IOException {
    // Arrange
    XzResource r1 = new XzResource(new Path(new Project(), "ant.antunit.log"));

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent8() throws IOException {
    // Arrange
    Project p = new Project();
    XzResource xzResource = new XzResource(new LogContent(p, new LogLevel()));

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(xzResource, new Resource("Name"), true));
    assertFalse(xzResource.isExists());
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent9() throws IOException {
    // Arrange
    XzResource r1 = new XzResource(new Resource("ant.antunit.log"));

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent10() throws IOException {
    // Arrange
    XzResource r1 = new XzResource(new Resource());

    // Act and Assert
    assertEquals(1, ResourceUtils.compareContent(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent11() throws IOException {
    // Arrange
    XzResource r1 = new XzResource(new LogContent());

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new XzResource(new LogContent()), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent12() throws IOException {
    // Arrange
    FileResource r1 = new FileResource();

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new FileResource(), true));
  }

  /**
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent13() throws IOException {
    // Arrange
    FileResource r1 = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1,
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals() throws IOException {
    // Arrange
    Resource r1 = new Resource("Name");

    // Act and Assert
    assertTrue(ResourceUtils.contentEquals(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals2() throws IOException {
    // Arrange
    Resource r1 = new Resource();

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals3() throws IOException {
    // Arrange
    Resource r1 = new Resource("Name", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals4() throws IOException {
    // Arrange
    JavaConstantResource r1 = new JavaConstantResource();

    // Act and Assert
    assertTrue(ResourceUtils.contentEquals(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals5() throws IOException {
    // Arrange
    Resource r1 = new Resource();

    // Act and Assert
    assertTrue(ResourceUtils.contentEquals(r1, new Resource(), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals6() throws IOException {
    // Arrange
    Resource r1 = new Resource();

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1,
        new Resource("Name", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, true), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals7() throws IOException {
    // Arrange
    XzResource r1 = new XzResource(new Path(new Project(), "ant.antunit.log"));

    // Act and Assert
    assertTrue(ResourceUtils.contentEquals(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals8() throws IOException {
    // Arrange
    Project p = new Project();
    XzResource xzResource = new XzResource(new LogContent(p, new LogLevel()));

    // Act and Assert
    assertTrue(ResourceUtils.contentEquals(xzResource, new Resource("Name"), true));
    assertFalse(xzResource.isExists());
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals9() throws IOException {
    // Arrange
    XzResource r1 = new XzResource(new Resource("ant.antunit.log"));

    // Act and Assert
    assertTrue(ResourceUtils.contentEquals(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals10() throws IOException {
    // Arrange
    XzResource r1 = new XzResource(new Resource());

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource("Name"), true));
  }

  /**
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals11() throws IOException {
    // Arrange
    Resource r1 = new Resource("(anonymous)", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, true);

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource(), true));
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource)}
   */
  @Test
  public void testCopyResource() throws IOException {
    // Arrange
    Resource resource = new Resource("Name");
    Resource resource1 = new Resource("Name");

    // Act
    ResourceUtils.copyResource(resource, resource1);

    // Assert that nothing has changed
    assertEquals(1, resource.size());
    assertEquals(0L, resource.getSize());
    assertEquals("Name", resource.getName());
    assertEquals(1, resource1.size());
    assertEquals(0L, resource1.getSize());
    assertEquals("Name", resource1.getName());
    Location expectedLocation = resource.getLocation();
    assertSame(expectedLocation, resource1.getLocation());
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource)}
   */
  @Test
  public void testCopyResource2() throws IOException {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    ResourceUtils.copyResource(fileResource, new Resource("Name"));

    // Assert that nothing has changed
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "\"");
    assertEquals(expectedToLongStringResult, fileResource.toLongString());
    assertEquals(1, fileResource.size());
    assertTrue(fileResource.isFilesystemOnly());
    assertEquals("test.txt", fileResource.getName());
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource)}
   */
  @Test
  public void testCopyResource3() throws IOException {
    // Arrange
    Resource resource = new Resource("Name", true, FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY);

    Resource resource1 = new Resource("Name", true, FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY);

    // Act
    ResourceUtils.copyResource(resource, resource1);

    // Assert that nothing has changed
    assertEquals(1, resource.size());
    assertEquals(-1L, resource.getSize());
    assertEquals("Name", resource.getName());
    assertEquals(FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY, resource.getLastModified());
    assertEquals(1, resource1.size());
    assertEquals(-1L, resource1.getSize());
    assertEquals("Name", resource1.getName());
    Location expectedLocation = resource.getLocation();
    assertSame(expectedLocation, resource1.getLocation());
    assertEquals(FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY, resource1.getLastModified());
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource)}
   */
  @Test
  public void testCopyResource4() throws IOException {
    // Arrange
    XzResource xzResource = new XzResource(new Path(new Project(), "ant.antunit.log"));
    Resource resource = new Resource("Name");

    // Act
    ResourceUtils.copyResource(xzResource, resource);

    // Assert that nothing has changed
    assertEquals(1, xzResource.size());
    assertFalse(xzResource.isExists());
    assertEquals(1, resource.size());
    assertEquals(0L, resource.getSize());
    assertEquals("Name", resource.getName());
    Location expectedLocation = xzResource.getLocation();
    assertSame(expectedLocation, resource.getLocation());
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource)}
   */
  @Test
  public void testCopyResource5() throws IOException {
    // Arrange
    Project p = new Project();
    XzResource xzResource = new XzResource(new LogContent(p, new LogLevel()));

    // Act
    ResourceUtils.copyResource(xzResource, new Resource("Name"));

    // Assert
    assertFalse(xzResource.isExists());
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource)}
   */
  @Test
  public void testCopyResource6() throws IOException {
    // Arrange
    XzResource xzResource = new XzResource(new Resource("ant.antunit.log"));
    Resource resource = new Resource("Name");

    // Act
    ResourceUtils.copyResource(xzResource, resource);

    // Assert that nothing has changed
    assertEquals(1, xzResource.size());
    assertFalse(xzResource.isExists());
    assertEquals(1, resource.size());
    assertEquals(0L, resource.getSize());
    assertEquals("Name", resource.getName());
    Location expectedLocation = xzResource.getLocation();
    assertSame(expectedLocation, resource.getLocation());
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource, Project)}
   */
  @Test
  public void testCopyResource7() throws IOException {
    // Arrange
    Resource resource = new Resource("Name");
    Resource resource1 = new Resource("Name");
    Project project = new Project();

    // Act
    ResourceUtils.copyResource(resource, resource1, project);

    // Assert that nothing has changed
    assertEquals(1, resource.size());
    assertEquals(0L, resource.getSize());
    assertEquals("Name", resource.getName());
    assertEquals(1, resource1.size());
    assertEquals(0L, resource1.getSize());
    assertEquals("Name", resource1.getName());
    Location expectedLocation = resource.getLocation();
    assertSame(expectedLocation, resource1.getLocation());
    assertTrue(project.getInputHandler() instanceof DefaultInputHandler);
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource, Project)}
   */
  @Test
  public void testCopyResource8() throws IOException {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    Resource dest = new Resource("Name");

    // Act
    ResourceUtils.copyResource(fileResource, dest, new Project());

    // Assert that nothing has changed
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "\"");
    assertEquals(expectedToLongStringResult, fileResource.toLongString());
    assertEquals(1, fileResource.size());
    assertTrue(fileResource.isFilesystemOnly());
    assertEquals("test.txt", fileResource.getName());
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource, Project)}
   */
  @Test
  public void testCopyResource9() throws IOException {
    // Arrange
    Resource resource = new Resource("Name", true, FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY);

    Resource resource1 = new Resource("Name", true, FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY);

    Project project = new Project();

    // Act
    ResourceUtils.copyResource(resource, resource1, project);

    // Assert that nothing has changed
    assertEquals(1, resource.size());
    assertEquals(-1L, resource.getSize());
    assertEquals("Name", resource.getName());
    assertEquals(FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY, resource.getLastModified());
    assertEquals(1, resource1.size());
    assertEquals(-1L, resource1.getSize());
    assertEquals("Name", resource1.getName());
    Location expectedLocation = resource.getLocation();
    assertSame(expectedLocation, resource1.getLocation());
    assertEquals(FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY, resource1.getLastModified());
    assertTrue(project.getInputHandler() instanceof DefaultInputHandler);
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource, Project)}
   */
  @Test
  public void testCopyResource10() throws IOException {
    // Arrange
    XzResource xzResource = new XzResource(new Path(new Project(), "ant.antunit.log"));
    Resource resource = new Resource("Name");

    // Act
    ResourceUtils.copyResource(xzResource, resource, new Project());

    // Assert that nothing has changed
    assertEquals(1, xzResource.size());
    assertFalse(xzResource.isExists());
    assertEquals(1, resource.size());
    assertEquals(0L, resource.getSize());
    assertEquals("Name", resource.getName());
    Location expectedLocation = xzResource.getLocation();
    assertSame(expectedLocation, resource.getLocation());
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource, Project)}
   */
  @Test
  public void testCopyResource11() throws IOException {
    // Arrange
    Project p = new Project();
    XzResource xzResource = new XzResource(new LogContent(p, new LogLevel()));
    Resource dest = new Resource("Name");

    // Act
    ResourceUtils.copyResource(xzResource, dest, new Project());

    // Assert
    assertFalse(xzResource.isExists());
  }

  /**
   * Method under test: {@link ResourceUtils#copyResource(Resource, Resource, Project)}
   */
  @Test
  public void testCopyResource12() throws IOException {
    // Arrange
    XzResource xzResource = new XzResource(new Resource("ant.antunit.log"));
    Resource resource = new Resource("Name");

    // Act
    ResourceUtils.copyResource(xzResource, resource, new Project());

    // Assert that nothing has changed
    assertEquals(1, xzResource.size());
    assertFalse(xzResource.isExists());
    assertEquals(1, resource.size());
    assertEquals(0L, resource.getSize());
    assertEquals("Name", resource.getName());
    Location expectedLocation = xzResource.getLocation();
    assertSame(expectedLocation, resource.getLocation());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource("ant.antunit.log");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources2() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources3() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource("ant.antunit.log", true, FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY);

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources4() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Project p = new Project();
    LogContent logContent = new LogContent(p, new LogLevel());

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, logContent,
        mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertEquals("LogContent \"(anonymous)\"", logContent.toLongString());
    assertFalse(logContent.isExists());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources5() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    Resource source = new Resource("ant.antunit.log");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources6() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Path path = new Path(new Project(), "dirs must be set to a positive number");

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, path, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertEquals(1, path.size());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources7() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource("ant.antunit.log");

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, null,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources8() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource("ant.antunit.log");
    FilterMapper mapper = new FilterMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources9() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource("ant.antunit.log");
    ChainedMapper mapper = new ChainedMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources10() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource("ant.antunit.log");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), Long.MAX_VALUE);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources11() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource();
    ChainedMapper mapper = new ChainedMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertEquals(1, ((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().size());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources12() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource("ant.antunit.log", true, Resource.UNKNOWN_SIZE);

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources13() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource("ant.antunit.log", true, FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY);

    FilterMapper mapper = new FilterMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertEquals(1, ((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().size());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources14() {
    // Arrange
    AntUnit logTo = new AntUnit();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    LogContent logContent = new LogContent(project, new LogLevel());

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, logContent,
        mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertEquals("LogContent \"(anonymous)\"", logContent.toLongString());
    assertFalse(logContent.isExists());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources15() {
    // Arrange
    AntUnit logTo = new AntUnit();

    Project project = new Project();
    project.addDataTypeDefinition("ant.antunit.log", Object.class);
    project.addBuildListener(new AntClassLoader());
    LogContent source = new LogContent(project, new LogLevel());

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, source, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources16() throws BuildException {
    // Arrange
    AntUnit logTo = new AntUnit();

    Project project = new Project();
    project.addTarget("ant.antunit.log", new Target());
    project.addBuildListener(new AntClassLoader());
    LogContent logContent = new LogContent(project, new LogLevel());

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo, logContent,
        mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertEquals("LogContent \"(anonymous)\"", logContent.toLongString());
    assertFalse(logContent.isExists());
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources17() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources18() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources19() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource(null)}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources20() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources21() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{null}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources22() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{
                new Resource("dirs must be set to a positive number", true, FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY)},
            mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources23() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())},
            mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources24() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new JavaConstantResource()}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources25() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources26() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource resource = new Resource("dirs must be set to a positive number");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{resource, new Resource("dirs must be set to a positive number")}, mapper,
            new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources27() {
    // Arrange
    AntUnit logTo = new AntUnit();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, null,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources28() {
    // Arrange
    AntUnit logTo = new AntUnit();
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources29() {
    // Arrange
    AntUnit logTo = new AntUnit();
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources30() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(antUnit, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources31() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{null}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources32() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources33() {
    // Arrange
    AntUnit logTo = new AntUnit();
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource(null)}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources34() {
    // Arrange
    AntUnit logTo = new AntUnit();

    Resource resource = new Resource("Name");
    resource.setProject(new Project());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{resource}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources35() {
    // Arrange
    AntUnit logTo = new AntUnit();
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(1, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources36() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new Resource("dirs must be set to a positive number", true, Resource.UNKNOWN_SIZE)}, mapper,
            new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources37() {
    // Arrange
    AntUnit logTo = new AntUnit();
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(1,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{
                new Resource("dirs must be set to a positive number", true, FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY)},
            mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources38() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo,
        new Resource[]{new XzResource(new Resource("ant.antunit.log"))}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources39() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new XzResource(new Path(new Project(), "ant.antunit.log"))}, mapper,
            new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources40() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Project p = new Project();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo,
        new Resource[]{new XzResource(new LogContent(p, new LogLevel()))}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources41() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo,
        new Resource[]{new FileResource(Paths.get(".", "test.txt").toFile())}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources42() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo,
        new Resource[]{new FileResource(Paths.get("..", "test.txt").toFile())}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources43() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile())}, mapper,
            new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources44() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())}, mapper,
            new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources45() {
    // Arrange
    AntUnit logTo = new AntUnit();
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new JavaConstantResource()}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSources46() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource resource = new Resource("Caught ");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{resource, new Resource("dirs must be set to a positive number")}, mapper,
            new DirectoryScanner()).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources47() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources48() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources49() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource(null)}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources50() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources51() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{null}, mapper, new DirectoryScanner(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources52() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{
                new Resource("dirs must be set to a positive number", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY)},
            mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources53() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())},
            mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources54() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new JavaConstantResource()}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources55() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{}, mapper, new DirectoryScanner(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources56() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource resource = new Resource("dirs must be set to a positive number");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{resource, new Resource("dirs must be set to a positive number")}, mapper,
            new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources57() {
    // Arrange
    AntUnit logTo = new AntUnit();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, null,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources58() {
    // Arrange
    AntUnit logTo = new AntUnit();
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources59() {
    // Arrange
    AntUnit logTo = new AntUnit();
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources60() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner(), Long.MAX_VALUE).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources61() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(antUnit, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources62() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{null}, mapper, new DirectoryScanner(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources63() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources64() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource("Name")}, mapper,
        new DirectoryScanner(), Long.MAX_VALUE).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources65() {
    // Arrange
    AntUnit logTo = new AntUnit();
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource(null)}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources66() {
    // Arrange
    AntUnit logTo = new AntUnit();

    Resource resource = new Resource("Name");
    resource.setProject(new Project());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{resource}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources67() {
    // Arrange
    AntUnit logTo = new AntUnit();
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(1, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources68() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new Resource("dirs must be set to a positive number", true, Resource.UNKNOWN_SIZE)}, mapper,
            new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources69() {
    // Arrange
    AntUnit logTo = new AntUnit();
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(1,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{
                new Resource("dirs must be set to a positive number", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY)},
            mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources70() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new XzResource(new Resource("ant.antunit.log"))},
            mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources71() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new XzResource(new Path(new Project(), "ant.antunit.log"))}, mapper, new DirectoryScanner(),
            FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources72() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Project p = new Project();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new XzResource(new LogContent(p, new LogLevel()))},
            mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources73() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new FileResource(Paths.get(".", "test.txt").toFile())}, mapper, new DirectoryScanner(),
            FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources74() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new FileResource(Paths.get("..", "test.txt").toFile())}, mapper, new DirectoryScanner(),
            FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources75() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile())}, mapper,
            new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources76() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())}, mapper,
            new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources77() {
    // Arrange
    AntUnit logTo = new AntUnit();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())},
            mapper, new DirectoryScanner(), Long.MAX_VALUE).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources78() {
    // Arrange
    AntUnit logTo = new AntUnit();
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new JavaConstantResource()}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSources79() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource resource = new Resource("Caught ");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo,
            new Resource[]{resource, new Resource("dirs must be set to a positive number")}, mapper,
            new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources() {
    // Arrange
    AntUnit logTo = new AntUnit();
    LogContent source = new LogContent();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources2() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    LogContent source = new LogContent();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources3() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Resource source = new Resource();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources4() {
    // Arrange
    AntUnit logTo = new AntUnit();
    Path path = new Path(new Project(), "dirs must be set to a positive number");

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, path, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertEquals(1, path.size());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources5() {
    // Arrange
    AntUnit logTo = new AntUnit();
    LogContent source = new LogContent();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, source, null,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources6() {
    // Arrange
    AntUnit logTo = new AntUnit();
    LogContent source = new LogContent();
    ScriptMapper mapper = new ScriptMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources7() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());
    LogContent source = new LogContent();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(antUnit, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources8() {
    // Arrange
    AntUnit logTo = new AntUnit();

    FileList.FileName fileName = new FileList.FileName();
    fileName.setName("");

    FileList fileList = new FileList();
    fileList.addConfiguredFile(fileName);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, fileList, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources9() throws BuildException {
    // Arrange
    AntUnit logTo = new AntUnit();

    Path path = new Path(new Project(), "dirs must be set to a positive number");
    path.add(new Path(new Project()));
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, path, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertEquals(1, path.size());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources10() {
    // Arrange
    AntUnit logTo = new AntUnit();

    Path path = new Path(new Project(), "dirs must be set to a positive number");
    path.addJavaRuntime();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, path, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertEquals(4, path.size());
  }

  /**
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceUtils.ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources11() {
    // Arrange
    AntUnit logTo = new AntUnit();

    Path path = new Path(new Project(), "dirs must be set to a positive number");
    path.addJavaRuntime();
    path.addJavaRuntime();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, path, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertEquals(0, actualSelectSourcesResult.size());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertEquals(4, path.size());
  }

  /**
   * Method under test: {@link ResourceUtils#setLastModified(Touchable, long)}
   */
  @Test
  public void testSetLastModified() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    ResourceUtils.setLastModified(fileResource, 10L);

    // Assert that nothing has changed
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "\"");
    assertEquals(expectedToLongStringResult, fileResource.toLongString());
    assertEquals(1, fileResource.size());
    assertTrue(fileResource.isFilesystemOnly());
    assertEquals("test.txt", fileResource.getName());
  }

  /**
   * Method under test: {@link ResourceUtils#setLastModified(Touchable, long)}
   */
  @Test
  public void testSetLastModified2() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setProject(new Project());

    // Act
    ResourceUtils.setLastModified(fileResource, 10L);

    // Assert that nothing has changed
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "\"");
    assertEquals(expectedToLongStringResult, fileResource.toLongString());
    assertEquals(1, fileResource.size());
    assertTrue(fileResource.isFilesystemOnly());
    assertEquals("test.txt", fileResource.getName());
  }

  /**
   * Method under test: {@link ResourceUtils#setLastModified(Touchable, long)}
   */
  @Test
  public void testSetLastModified3() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setProject(project);

    // Act
    ResourceUtils.setLastModified(fileResource, 10L);

    // Assert that nothing has changed
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "\"");
    assertEquals(expectedToLongStringResult, fileResource.toLongString());
    assertEquals(1, fileResource.size());
    assertTrue(fileResource.isFilesystemOnly());
    assertEquals("test.txt", fileResource.getName());
  }

  /**
   * Method under test: {@link ResourceUtils#setLastModified(Touchable, long)}
   */
  @Test
  public void testSetLastModified4() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("Failed to change file modification time", Object.class);
    project.addBuildListener(new AntClassLoader());

    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setProject(project);

    // Act
    ResourceUtils.setLastModified(fileResource, 10L);

    // Assert that nothing has changed
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "\"");
    assertEquals(expectedToLongStringResult, fileResource.toLongString());
    assertEquals(1, fileResource.size());
    assertTrue(fileResource.isFilesystemOnly());
    assertEquals("test.txt", fileResource.getName());
  }

  /**
   * Method under test: {@link ResourceUtils#setLastModified(Touchable, long)}
   */
  @Test
  public void testSetLastModified5() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(".");

    // Act
    ResourceUtils.setLastModified(fileResource, 10L);

    // Assert
    assertFalse(fileResource.isDirectory());
  }

  /**
   * Method under test: {@link ResourceUtils#setLastModified(Touchable, long)}
   */
  @Test
  public void testSetLastModified6() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setProject(project);

    // Act
    ResourceUtils.setLastModified(fileResource, 10L);

    // Assert that nothing has changed
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "\"");
    assertEquals(expectedToLongStringResult, fileResource.toLongString());
    assertEquals(1, fileResource.size());
    assertTrue(fileResource.isFilesystemOnly());
    assertEquals("test.txt", fileResource.getName());
  }

  /**
   * Method under test: {@link ResourceUtils#setLastModified(Touchable, long)}
   */
  @Test
  public void testSetLastModified7() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act
    ResourceUtils.setLastModified(fileResource, 10L);

    // Assert
    assertFalse(fileResource.isDirectory());
  }

  /**
   * Method under test: {@link ResourceUtils#setLastModified(Touchable, long)}
   */
  @Test
  public void testSetLastModified8() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new CommonsLoggingListener());

    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setProject(project);

    // Act
    ResourceUtils.setLastModified(fileResource, 10L);

    // Assert that nothing has changed
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "\"");
    assertEquals(expectedToLongStringResult, fileResource.toLongString());
    assertEquals(1, fileResource.size());
    assertTrue(fileResource.isFilesystemOnly());
    assertEquals("test.txt", fileResource.getName());
  }
}

