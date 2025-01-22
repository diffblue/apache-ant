package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.ResourceFactory;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.optional.ScriptMapper;
import org.apache.tools.ant.types.resources.FileProvider;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.LogOutputResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.apache.tools.ant.types.resources.Resources;
import org.apache.tools.ant.types.resources.Union;
import org.apache.tools.ant.util.ResourceUtils.ReadOnlyTargetFileException;
import org.apache.tools.ant.util.ResourceUtils.ResourceSelectorProvider;
import org.apache.tools.ant.util.facade.ImplementationSpecificArgument;
import org.junit.Test;

public class ResourceUtilsDiffblueTest {
  /**
   * Test ReadOnlyTargetFileException {@link ReadOnlyTargetFileException#ReadOnlyTargetFileException(File)}.
   * <p>
   * Method under test: {@link ReadOnlyTargetFileException#ReadOnlyTargetFileException(File)}
   */
  @Test
  public void testReadOnlyTargetFileExceptionNewReadOnlyTargetFileException() {
    // Arrange and Act
    ReadOnlyTargetFileException actualReadOnlyTargetFileException = new ReadOnlyTargetFileException(
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertNull(actualReadOnlyTargetFileException.getCause());
    assertEquals(0, actualReadOnlyTargetFileException.getSuppressed().length);
    assertEquals(
        String.join("", "can't write to read-only destination file ",
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()),
        actualReadOnlyTargetFileException.getLocalizedMessage());
    assertEquals(
        String.join("", "can't write to read-only destination file ",
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()),
        actualReadOnlyTargetFileException.getMessage());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertTrue(
        ResourceUtils
            .selectOutOfDateSources(logTo, Resources.NONE, mapper, new DirectoryScanner(),
                FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY)
            .isEmpty());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong2() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertTrue(
        ResourceUtils
            .selectOutOfDateSources(logTo, Resources.NONE, mapper, new DirectoryScanner(),
                FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY)
            .isEmpty());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong3() {
    // Arrange
    Concat source = new Concat();
    source.addFilelist(new FileList());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong4() {
    // Arrange
    Concat source = new Concat();
    source.addText("At least one resource must be provided, or some text.");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong5() {
    // Arrange
    Concat source = new Concat();
    source.setDest(new Resource());
    source.addFilelist(new FileList());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong6() {
    // Arrange
    Concat source = new Concat();
    source.setResourceName("concat (");
    source.addFilelist(new FileList());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong7() {
    // Arrange
    Concat source = new Concat();
    source.setProject(new Project());
    source.addFilelist(new FileList());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong8() {
    // Arrange
    Concat source = new Concat();
    source.setProject(new Project());
    source.addText("At least one resource must be provided, or some text.");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong9() {
    // Arrange
    FileName name = new FileName();
    name.setName("No sources found.");

    FileList source = new FileList();
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong10() {
    // Arrange
    FileName name = new FileName();
    name.setName("No sources found.");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList source = new FileList();
    source.addConfiguredFile(name2);
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong11() {
    // Arrange
    FileName name = new FileName();
    name.setName(".");

    FileList source = new FileList();
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong12() {
    // Arrange
    FileName name = new FileName();
    name.setName("..");

    FileList source = new FileList();
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong13() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList source = new FileList();
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong14()
      throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("No sources found.");

    FileList source = new FileList();
    source.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code ResourceCollection}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceCollectionFileNameMapperResourceFactoryLong15() {
    // Arrange
    FileName name = new FileName();
    name.setName("No sources found.");

    FileName name2 = new FileName();
    name2.setName("No sources found.");

    FileList source = new FileList();
    source.addConfiguredFile(name2);
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(
        Path.systemBootClasspath, source, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertTrue(actualSelectOutOfDateSourcesResult instanceof Union);
    Location location = ((Union) actualSelectOutOfDateSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getDescription());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getProject());
    assertNull(((Union) actualSelectOutOfDateSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectOutOfDateSourcesResult.size());
    assertFalse(((Union) actualSelectOutOfDateSourcesResult).isReference());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectOutOfDateSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new Resource()},
        mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory2() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory3() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory4() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{null}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory5() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{new Resource("dirs must be set to a positive number")}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory6() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{
                new Resource("dirs must be set to a positive number", true, FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY)},
            mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory7() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())},
            mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory8() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{new JavaConstantResource()}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory9() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory10() {
    // Arrange, Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new Resource()}, null,
        new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory11() {
    // Arrange
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new Resource()},
        mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory12() {
    // Arrange
    ScriptMapper mapper = new ScriptMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new Resource()},
        mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory13() {
    // Arrange
    Resource resource = new Resource();
    ChainedMapper mapper = new ChainedMapper();

    // Act
    Resource[] actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{resource}, mapper, new DirectoryScanner());

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.length);
    assertSame(resource, actualSelectOutOfDateSourcesResult[0]);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory14() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{null}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory15() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    Resource resource = new Resource();
    ChainedMapper mapper = new ChainedMapper();

    // Act
    Resource[] actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo,
        new Resource[]{resource}, mapper, new DirectoryScanner());

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.length);
    assertSame(resource, actualSelectOutOfDateSourcesResult[0]);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory16() {
    // Arrange
    Resource resource = new Resource();
    resource.setProject(new Project());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{resource}, mapper,
        new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory17() {
    // Arrange
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{new Resource("dirs must be set to a positive number")}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory18() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new Resource("dirs must be set to a positive number", true, Resource.UNKNOWN_SIZE)}, mapper,
            new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory19() {
    // Arrange
    Resource resource = new Resource("dirs must be set to a positive number", true,
        FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY);

    FilterMapper mapper = new FilterMapper();

    // Act
    Resource[] actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{resource}, mapper, new DirectoryScanner());

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.length);
    assertSame(resource, actualSelectOutOfDateSourcesResult[0]);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory20() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile())}, mapper,
            new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory21() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())}, mapper,
            new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory22() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    FilterMapper mapper = new FilterMapper();

    // Act
    Resource[] actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{fileResource}, mapper, new DirectoryScanner());

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.length);
    assertSame(fileResource, actualSelectOutOfDateSourcesResult[0]);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory23() {
    // Arrange
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{new JavaConstantResource()}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory24() {
    // Arrange
    Resource resource = new Resource("dirs must be set to a positive number");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{resource, new Resource()}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactory25() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{javaConstantResource, new Resource()}, mapper, new DirectoryScanner()).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new Resource()},
        mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong2() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong3() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong4() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{null}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong5() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new Resource("dirs must be set to a positive number")}, mapper, new DirectoryScanner(),
            FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong6() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{
                new Resource("dirs must be set to a positive number", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY)},
            mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong7() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())},
            mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong8() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new JavaConstantResource()},
            mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong9() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong10() {
    // Arrange, Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new Resource()}, null,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong11() {
    // Arrange
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new Resource()},
        mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong12() {
    // Arrange
    ScriptMapper mapper = new ScriptMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new Resource()},
        mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong13() {
    // Arrange
    Resource resource = new Resource();
    ChainedMapper mapper = new ChainedMapper();

    // Act
    Resource[] actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{resource}, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.length);
    assertSame(resource, actualSelectOutOfDateSourcesResult[0]);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong14() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new Resource()},
        mapper, new DirectoryScanner(), Long.MAX_VALUE).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong15() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner(), Long.MAX_VALUE).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong16() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{null}, mapper, new DirectoryScanner(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong17() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    Resource resource = new Resource();
    ChainedMapper mapper = new ChainedMapper();

    // Act
    Resource[] actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(logTo,
        new Resource[]{resource}, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.length);
    assertSame(resource, actualSelectOutOfDateSourcesResult[0]);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong18() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(logTo, new Resource[]{new Resource()}, mapper,
        new DirectoryScanner(), Long.MAX_VALUE).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong19() {
    // Arrange
    Resource resource = new Resource();
    resource.setProject(new Project());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0, ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{resource}, mapper,
        new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong20() {
    // Arrange
    FilterMapper mapper = new FilterMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new Resource("dirs must be set to a positive number")}, mapper, new DirectoryScanner(),
            FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong21() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new Resource("dirs must be set to a positive number", true, Resource.UNKNOWN_SIZE)}, mapper,
            new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong22() {
    // Arrange
    Resource resource = new Resource("dirs must be set to a positive number", true,
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    FilterMapper mapper = new FilterMapper();

    // Act
    Resource[] actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{resource}, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.length);
    assertSame(resource, actualSelectOutOfDateSourcesResult[0]);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong23() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile())}, mapper,
            new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong24() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())}, mapper,
            new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong25() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    FilterMapper mapper = new FilterMapper();

    // Act
    Resource[] actualSelectOutOfDateSourcesResult = ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
        new Resource[]{fileResource}, mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Assert
    assertEquals(1, actualSelectOutOfDateSourcesResult.length);
    assertSame(fileResource, actualSelectOutOfDateSourcesResult[0]);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong26() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())},
            mapper, new DirectoryScanner(), Long.MAX_VALUE).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong27() {
    // Arrange
    ChainedMapper mapper = new ChainedMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{new JavaConstantResource()},
            mapper, new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong28() {
    // Arrange
    Resource resource = new Resource("dirs must be set to a positive number");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath, new Resource[]{resource, new Resource()}, mapper,
            new DirectoryScanner(), FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)} with {@code ProjectComponent}, {@code Resource[]}, {@code FileNameMapper}, {@code ResourceFactory}, {@code long}.
   * <p>
   * Method under test: {@link ResourceUtils#selectOutOfDateSources(ProjectComponent, Resource[], FileNameMapper, ResourceFactory, long)}
   */
  @Test
  public void testSelectOutOfDateSourcesWithProjectComponentResourceFileNameMapperResourceFactoryLong29() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertEquals(0,
        ResourceUtils.selectOutOfDateSources(Path.systemBootClasspath,
            new Resource[]{javaConstantResource, new Resource()}, mapper, new DirectoryScanner(),
            FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY).length);
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@code concat (}.</li>
   *   <li>When {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenConcat_whenConcatResourceNameIsConcat_thenReturnUnion() {
    // Arrange
    Concat source = new Concat();
    source.setResourceName("concat (");
    source.addFilelist(new FileList());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   *   <li>When {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenFileList_whenConcatAddFilelistFileList_thenReturnUnion() {
    // Arrange
    Concat source = new Concat();
    source.addFilelist(new FileList());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenFileNameNameIsDotDot() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileName name2 = new FileName();
    name2.setName("..");

    FileList source = new FileList();
    source.addConfiguredFile(name2);
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenFileNameNameIsDotDot2() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileName name2 = new FileName();
    name2.setName("..");

    FileList source = new FileList();
    source.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    source.addConfiguredFile(name2);
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code .}.</li>
   *   <li>When {@link FileList#FileList()} addConfiguredFile {@link FileName} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenFileNameNameIsDot_whenFileListAddConfiguredFileFileName() {
    // Arrange
    FileName name = new FileName();
    name.setName(".");

    FileList source = new FileList();
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenFileNameNameIsEmptyString() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList source = new FileList();
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>When {@link FileList#FileList()} addConfiguredFile {@link FileName} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenFileNameNameIsName_whenFileListAddConfiguredFileFileName() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList source = new FileList();
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>When {@link FileList#FileList()} addConfiguredFile {@link FileName} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenFileNameNameIsName_whenFileListAddConfiguredFileFileName2() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileName name2 = new FileName();
    name2.setName("Name");

    FileList source = new FileList();
    source.addConfiguredFile(name2);
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenProject_whenConcatProjectIsProject_thenReturnUnion() {
    // Arrange
    Concat source = new Concat();
    source.setProject(new Project());
    source.addText("Text");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given Property is {@code java.io.tmpdir} is array of {@link String} with {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenPropertyIsJavaIoTmpdirIsArrayOfStringWithTestTxtToFile() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList source = new FileList();
    source.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    source.addConfiguredFile(name);
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenResource_whenConcatDestIsResource_thenReturnUnion() {
    // Arrange
    Concat source = new Concat();
    source.setDest(new Resource());
    source.addFilelist(new FileList());
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>Given {@code Text}.</li>
   *   <li>When {@link Concat} (default constructor) addText {@code Text}.</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_givenText_whenConcatAddTextText_thenReturnUnion() {
    // Arrange
    Concat source = new Concat();
    source.addText("Text");
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_whenFileList_thenReturnEmpty() {
    // Arrange
    FileList source = new FileList();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertTrue(
        ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper, new DirectoryScanner(), null).isEmpty());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>When {@link ImplementationSpecificArgument} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_whenImplementationSpecificArgument_thenReturnEmpty() {
    // Arrange
    ImplementationSpecificArgument logTo = new ImplementationSpecificArgument();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertTrue(ResourceUtils.selectSources(logTo, Resources.NONE, mapper, new DirectoryScanner(), null).isEmpty());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_whenNone_thenReturnEmpty() {
    // Arrange
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertTrue(
        ResourceUtils.selectSources(Path.systemBootClasspath, Resources.NONE, mapper, new DirectoryScanner(), null)
            .isEmpty());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_whenPathWithPIsProjectAndPath_thenReturnUnion() {
    // Arrange
    Path source = new Path(new Project(), "Path");

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_whenPathWithPIsProjectAndPath_thenReturnUnion2() {
    // Arrange
    Path source = new Path(new Project(), "Path");

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, null,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_whenResource_thenReturnUnion() {
    // Arrange
    Resource source = new Resource();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>When {@link ScriptMapper} (default constructor).</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_whenScriptMapper_thenReturnUnion() {
    // Arrange
    Path source = new Path(new Project(), "Path");

    ScriptMapper mapper = new ScriptMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(Path.systemBootClasspath, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>When {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_whenTaskAdapter_thenReturnEmpty() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    CutDirsMapper mapper = new CutDirsMapper();

    // Act and Assert
    assertTrue(ResourceUtils.selectSources(logTo, Resources.NONE, mapper, new DirectoryScanner(), null).isEmpty());
  }

  /**
   * Test {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}.
   * <ul>
   *   <li>When {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return {@link Union}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#selectSources(ProjectComponent, ResourceCollection, FileNameMapper, ResourceFactory, ResourceSelectorProvider)}
   */
  @Test
  public void testSelectSources_whenTaskAdapter_thenReturnUnion() {
    // Arrange
    TaskAdapter logTo = new TaskAdapter();
    Path source = new Path(new Project(), "Path");

    CutDirsMapper mapper = new CutDirsMapper();

    // Act
    ResourceCollection actualSelectSourcesResult = ResourceUtils.selectSources(logTo, source, mapper,
        new DirectoryScanner(), null);

    // Assert
    assertTrue(actualSelectSourcesResult instanceof Union);
    Location location = ((Union) actualSelectSourcesResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualSelectSourcesResult).getDescription());
    assertNull(((Union) actualSelectSourcesResult).getProject());
    assertNull(((Union) actualSelectSourcesResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualSelectSourcesResult.size());
    assertFalse(((Union) actualSelectSourcesResult).isReference());
    assertTrue(((Union) actualSelectSourcesResult).getResourceCollections().isEmpty());
    assertTrue(((Union) actualSelectSourcesResult).isCache());
  }

  /**
   * Test {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals_givenFileAttributeIsNull() throws IOException {
    // Arrange
    FileResource r1 = new FileResource();
    r1.setName("file attribute is null!");

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals_givenTrue_whenResourceDirectoryIsTrue_thenReturnFalse() throws IOException {
    // Arrange
    Resource r1 = new Resource();
    r1.setDirectory(true);

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals_thenReturnTrue() throws IOException {
    // Arrange
    Resource r1 = new Resource("Name", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY);

    // Act and Assert
    assertTrue(
        ResourceUtils.contentEquals(r1, new Resource("Name", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY), true));
  }

  /**
   * Test {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() throws IOException {
    // Arrange
    FileResource r1 = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile2() throws IOException {
    // Arrange
    Resource r1 = new Resource();

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1,
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()), true));
  }

  /**
   * Test {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals_whenJavaConstantResource_thenReturnFalse() throws IOException {
    // Arrange
    JavaConstantResource r1 = new JavaConstantResource();

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link FileResource#FileResource(File)} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals_whenMappedResourceWithRIsFileResourceAndMIsCutDirsMapper() throws IOException {
    // Arrange
    FileResource r = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    MappedResource r1 = new MappedResource(r, new CutDirsMapper());

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals_whenResourceWithName_thenReturnFalse() throws IOException {
    // Arrange
    Resource r1 = new Resource("Name");

    // Act and Assert
    assertFalse(ResourceUtils.contentEquals(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#contentEquals(Resource, Resource, boolean)}
   */
  @Test
  public void testContentEquals_whenResourceWithName_thenReturnTrue() throws IOException {
    // Arrange
    Resource r1 = new Resource("Name");

    // Act and Assert
    assertTrue(ResourceUtils.contentEquals(r1, new Resource("Name"), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent() throws IOException {
    // Arrange
    LogOutputResource r1 = new LogOutputResource(Path.systemBootClasspath, 1);

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, ResourceUtils.compareContent(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent2() throws IOException {
    // Arrange
    FileResource r1 = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER,
        ResourceUtils.compareContent(r1, new Resource("Name", true, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent3() throws IOException {
    // Arrange
    LogOutputResource r1 = new LogOutputResource(Path.systemBootClasspath, 1);

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new LogOutputResource(Path.systemBootClasspath, 1), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_givenFileAttributeIsNull() throws IOException {
    // Arrange
    FileResource r1 = new FileResource();
    r1.setName("file attribute is null!");

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, ResourceUtils.compareContent(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_givenTrue_whenResourceDirectoryIsTrue_thenReturnZero() throws IOException {
    // Arrange
    FileResource r1 = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Resource r2 = new Resource();
    r2.setDirectory(true);

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, r2, true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_thenReturnOne() throws IOException {
    // Arrange
    Resource r1 = new Resource();

    // Act and Assert
    assertEquals(1, ResourceUtils.compareContent(r1,
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_thenReturnRetry_forever() throws IOException {
    // Arrange
    FileResource r1 = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, ResourceUtils.compareContent(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_thenReturnZero() throws IOException {
    // Arrange
    FileResource r1 = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1,
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_whenFileResource_thenReturnZero() throws IOException {
    // Arrange
    FileResource r1 = new FileResource();

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new FileResource(), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_whenJavaConstantResource_thenReturnOne() throws IOException {
    // Arrange
    Resource r1 = new Resource();

    // Act and Assert
    assertEquals(1, ResourceUtils.compareContent(r1, new JavaConstantResource(), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_whenJavaConstantResource_thenReturnRetry_forever() throws IOException {
    // Arrange
    JavaConstantResource r1 = new JavaConstantResource();

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, ResourceUtils.compareContent(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link FileResource#FileResource(File)} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_whenMappedResourceWithRIsFileResourceAndMIsCutDirsMapper() throws IOException {
    // Arrange
    FileResource r = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    MappedResource r1 = new MappedResource(r, new CutDirsMapper());

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, ResourceUtils.compareContent(r1, new Resource(), true));
  }

  /**
   * Test {@link ResourceUtils#compareContent(Resource, Resource, boolean)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#compareContent(Resource, Resource, boolean)}
   */
  @Test
  public void testCompareContent_whenResourceWithName_thenReturnZero() throws IOException {
    // Arrange
    JavaConstantResource r1 = new JavaConstantResource();

    // Act and Assert
    assertEquals(0, ResourceUtils.compareContent(r1, new Resource("Name"), true));
  }

  /**
   * Test {@link ResourceUtils#asFileResource(FileProvider)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceUtils#asFileResource(FileProvider)}
   */
  @Test
  public void testAsFileResource_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(ResourceUtils.asFileResource(null));
  }
}
