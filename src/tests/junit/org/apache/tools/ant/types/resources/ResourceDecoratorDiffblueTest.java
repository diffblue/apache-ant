package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Stack;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.junit.Test;

public class ResourceDecoratorDiffblueTest {
  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    Concat a = new Concat();
    a.addFilelist(new FileList());

    // Act
    bZip2Resource.addConfigured(a);

    // Assert
    assertEquals("BZip2Resource \"Bzip2 compressed concat ()\"", bZip2Resource.toLongString());
    Resource resource = bZip2Resource.getResource();
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
    assertEquals("concat ()", bZip2Resource.getName());
  }

  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured2() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    Concat a = new Concat();
    a.addText("At least one resource must be provided, or some text.");

    // Act
    bZip2Resource.addConfigured(a);

    // Assert
    assertEquals("BZip2Resource \"Bzip2 compressed concat (At least one resource must be provided, or some text.)\"",
        bZip2Resource.toLongString());
    Resource resource = bZip2Resource.getResource();
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        resource.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", resource.getName());
    assertEquals("concat (At least one resource must be provided, or some text.)", bZip2Resource.getName());
  }

  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link BZip2Resource#BZip2Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileList_whenConcatDestIsBZip2Resource() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    Concat a = new Concat();
    a.setDest(new BZip2Resource());
    a.addFilelist(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.addConfigured(a));
  }

  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resources#NONE}.</li>
   *   <li>When {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenNone_whenAllButFirstAddNone_thenThrowBuildException() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    AllButFirst a = new AllButFirst();
    a.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.addConfigured(a));
  }

  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link Resource#Resource(String)} with {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenResourceWithName_whenConcatDestIsResourceWithName() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    Concat a = new Concat();
    a.setDest(new Resource("Name"));
    a.addFilelist(new FileList());

    // Act
    bZip2Resource.addConfigured(a);

    // Assert
    assertEquals("BZip2Resource \"Bzip2 compressed concat ()\"", bZip2Resource.toLongString());
    Resource resource = bZip2Resource.getResource();
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
    assertEquals("concat ()", bZip2Resource.getName());
  }

  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenResource_whenConcatDestIsResource() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    Concat a = new Concat();
    a.setDest(new Resource());
    a.addFilelist(new FileList());

    // Act
    bZip2Resource.addConfigured(a);

    // Assert
    assertEquals("BZip2Resource \"Bzip2 compressed concat ()\"", bZip2Resource.toLongString());
    Resource resource = bZip2Resource.getResource();
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
    assertEquals("concat ()", bZip2Resource.getName());
  }

  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link AllButFirst} (default constructor) add {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenAllButFirstAddBZip2Resource_thenThrowBuildException() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    AllButFirst a = new AllButFirst();
    a.add(new BZip2Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.addConfigured(a));
  }

  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Archives} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenArchives_thenThrowBuildException() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.addConfigured(new Archives()));
  }

  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Files#Files()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenFiles_thenThrowBuildException() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.addConfigured(new Files()));
  }

  /**
   * Test {@link ResourceDecorator#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenNone_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).addConfigured(Resources.NONE));
  }

  /**
   * Test {@link ResourceDecorator#getName()}.
   * <p>
   * Method under test: {@link ResourceDecorator#getName()}
   */
  @Test
  public void testGetName() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.getName());
  }

  /**
   * Test {@link ResourceDecorator#getName()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource(ResourceCollection)} with other is {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getName()}
   */
  @Test
  public void testGetName_givenBZip2ResourceWithOtherIsBZip2Resource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource(new BZip2Resource())).getName());
  }

  /**
   * Test {@link ResourceDecorator#getName()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getName()}
   */
  @Test
  public void testGetName_givenBZip2Resource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).getName());
  }

  /**
   * Test {@link ResourceDecorator#getName()}.
   * <ul>
   *   <li>Then return {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getName()}
   */
  @Test
  public void testGetName_thenReturnFileAttributeIsNull() throws BuildException {
    // Arrange
    FileResource other = new FileResource();
    other.setName("file attribute is null!");

    BZip2Resource bZip2Resource = new BZip2Resource(other);
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertEquals("file attribute is null!", bZip2Resource.getName());
  }

  /**
   * Test {@link ResourceDecorator#getName()}.
   * <ul>
   *   <li>Then return {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getName()}
   */
  @Test
  public void testGetName_thenReturnTestTxt() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertEquals("test.txt", bZip2Resource.getName());
  }

  /**
   * Test {@link ResourceDecorator#isExists()}.
   * <p>
   * Method under test: {@link ResourceDecorator#isExists()}
   */
  @Test
  public void testIsExists() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.isExists());
  }

  /**
   * Test {@link ResourceDecorator#isExists()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource(ResourceCollection)} with other is {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#isExists()}
   */
  @Test
  public void testIsExists_givenBZip2ResourceWithOtherIsBZip2Resource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource(new BZip2Resource())).isExists());
  }

  /**
   * Test {@link ResourceDecorator#isExists()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#isExists()}
   */
  @Test
  public void testIsExists_givenBZip2Resource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).isExists());
  }

  /**
   * Test {@link ResourceDecorator#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#isExists()}
   */
  @Test
  public void testIsExists_givenFileResourceNameIsFileAttributeIsNull_thenReturnFalse() throws BuildException {
    // Arrange
    FileResource other = new FileResource();
    other.setName("file attribute is null!");

    BZip2Resource bZip2Resource = new BZip2Resource(other);
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertFalse(bZip2Resource.isExists());
  }

  /**
   * Test {@link ResourceDecorator#isExists()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#isExists()}
   */
  @Test
  public void testIsExists_thenReturnTrue() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertTrue(bZip2Resource.isExists());
  }

  /**
   * Test {@link ResourceDecorator#getLastModified()}.
   * <p>
   * Method under test: {@link ResourceDecorator#getLastModified()}
   */
  @Test
  public void testGetLastModified() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.getLastModified());
  }

  /**
   * Test {@link ResourceDecorator#getLastModified()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource(ResourceCollection)} with other is {@link BZip2Resource#BZip2Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenBZip2ResourceWithOtherIsBZip2Resource() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource(new BZip2Resource())).getLastModified());
  }

  /**
   * Test {@link ResourceDecorator#getLastModified()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenBZip2Resource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).getLastModified());
  }

  /**
   * Test {@link ResourceDecorator#getLastModified()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenFileResourceNameIsFileAttributeIsNull_thenReturnZero() throws BuildException {
    // Arrange
    FileResource other = new FileResource();
    other.setName("file attribute is null!");

    BZip2Resource bZip2Resource = new BZip2Resource(other);
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertEquals(0L, bZip2Resource.getLastModified());
  }

  /**
   * Test {@link ResourceDecorator#isDirectory()}.
   * <p>
   * Method under test: {@link ResourceDecorator#isDirectory()}
   */
  @Test
  public void testIsDirectory() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.isDirectory());
  }

  /**
   * Test {@link ResourceDecorator#isDirectory()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource(ResourceCollection)} with other is {@link BZip2Resource#BZip2Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenBZip2ResourceWithOtherIsBZip2Resource() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource(new BZip2Resource())).isDirectory());
  }

  /**
   * Test {@link ResourceDecorator#isDirectory()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenBZip2Resource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).isDirectory());
  }

  /**
   * Test {@link ResourceDecorator#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResourceNameIsFileAttributeIsNull_thenReturnFalse() throws BuildException {
    // Arrange
    FileResource other = new FileResource();
    other.setName("file attribute is null!");

    BZip2Resource bZip2Resource = new BZip2Resource(other);
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertFalse(bZip2Resource.isDirectory());
  }

  /**
   * Test {@link ResourceDecorator#isDirectory()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#isDirectory()}
   */
  @Test
  public void testIsDirectory_thenReturnTrue() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertTrue(bZip2Resource.isDirectory());
  }

  /**
   * Test {@link ResourceDecorator#getSize()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getSize()}
   */
  @Test
  public void testGetSize_givenFileResourceNameIsFileAttributeIsNull_thenReturnZero() {
    // Arrange
    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertEquals(0L, (new MappedResource(r, new CutDirsMapper())).getSize());
  }

  /**
   * Test {@link ResourceDecorator#getSize()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link JavaConstantResource} (default constructor) and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getSize()}
   */
  @Test
  public void testGetSize_givenMappedResourceWithRIsJavaConstantResourceAndMIsCutDirsMapper() {
    // Arrange
    JavaConstantResource r = new JavaConstantResource();

    // Act and Assert
    assertEquals(0L, (new MappedResource(r, new CutDirsMapper())).getSize());
  }

  /**
   * Test {@link ResourceDecorator#getSize()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link MappedResource#MappedResource(Resource, FileNameMapper)} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getSize()}
   */
  @Test
  public void testGetSize_givenMappedResourceWithRIsMappedResourceAndMIsCutDirsMapper() {
    // Arrange
    Resource r = new Resource();
    MappedResource r2 = new MappedResource(r, new CutDirsMapper());

    // Act and Assert
    assertEquals(-1L, (new MappedResource(r2, new CutDirsMapper())).getSize());
  }

  /**
   * Test {@link ResourceDecorator#getSize()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getSize()}
   */
  @Test
  public void testGetSize_givenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertEquals(-1L, (new MappedResource(r, new CutDirsMapper())).getSize());
  }

  /**
   * Test {@link ResourceDecorator#getSize()}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String, boolean, long)} with {@code Name} and exists is {@code true} and lastmodified is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getSize()}
   */
  @Test
  public void testGetSize_givenResourceWithNameAndExistsIsTrueAndLastmodifiedIsOne() {
    // Arrange
    Resource r = new Resource("Name", true, 1L);

    // Act and Assert
    assertEquals(-1L, (new MappedResource(r, new CutDirsMapper())).getSize());
  }

  /**
   * Test {@link ResourceDecorator#getSize()}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getSize()}
   */
  @Test
  public void testGetSize_givenResourceWithName_thenReturnZero() {
    // Arrange
    Resource r = new Resource("Name");

    // Act and Assert
    assertEquals(0L, (new MappedResource(r, new CutDirsMapper())).getSize());
  }

  /**
   * Test {@link ResourceDecorator#getSize()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getSize()}
   */
  @Test
  public void testGetSize_thenThrowBuildException() {
    // Arrange
    BZip2Resource r = new BZip2Resource();

    // Act and Assert
    assertThrows(BuildException.class, () -> (new MappedResource(r, new CutDirsMapper())).getSize());
  }

  /**
   * Test {@link ResourceDecorator#getInputStream()}.
   * <ul>
   *   <li>Then return available is {@link Double#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getInputStream()}
   */
  @Test
  public void testGetInputStream_thenReturnAvailableIsSize() throws IOException {
    // Arrange
    FileResource r = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(Double.SIZE, (new MappedResource(r, new CutDirsMapper())).getInputStream().available());
  }

  /**
   * Test {@link ResourceDecorator#getInputStream()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getInputStream()}
   */
  @Test
  public void testGetInputStream_thenThrowBuildException() throws IOException {
    // Arrange
    BZip2Resource r = new BZip2Resource();

    // Act and Assert
    assertThrows(BuildException.class, () -> (new MappedResource(r, new CutDirsMapper())).getInputStream());
  }

  /**
   * Test {@link ResourceDecorator#getOutputStream()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getOutputStream()}
   */
  @Test
  public void testGetOutputStream_thenThrowBuildException() throws IOException {
    // Arrange
    BZip2Resource r = new BZip2Resource();

    // Act and Assert
    assertThrows(BuildException.class, () -> (new MappedResource(r, new CutDirsMapper())).getOutputStream());
  }

  /**
   * Test {@link ResourceDecorator#isFilesystemOnly()}.
   * <p>
   * Method under test: {@link ResourceDecorator#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly() {
    // Arrange, Act and Assert
    assertFalse((new BZip2Resource()).isFilesystemOnly());
  }

  /**
   * Test {@link ResourceDecorator#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then {@link BZip2Resource#BZip2Resource()} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenBZip2Resource_thenBZip2ResourceReference() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();
    Reference r = new Reference("42");

    // Act
    bZip2Resource.setRefid(r);

    // Assert
    assertTrue(bZip2Resource.isReference());
    assertSame(r, bZip2Resource.getRefid());
  }

  /**
   * Test {@link ResourceDecorator#as(Class)}.
   * <p>
   * Method under test: {@link ResourceDecorator#as(Class)}
   */
  @Test
  public void testAs() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.as(clazz));
  }

  /**
   * Test {@link ResourceDecorator#as(Class)}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource(ResourceCollection)} with other is {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#as(Class)}
   */
  @Test
  public void testAs_givenBZip2ResourceWithOtherIsBZip2Resource_thenThrowBuildException() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.as(clazz));
  }

  /**
   * Test {@link ResourceDecorator#as(Class)}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#as(Class)}
   */
  @Test
  public void testAs_givenBZip2Resource_thenThrowBuildException() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.as(clazz));
  }

  /**
   * Test {@link ResourceDecorator#as(Class)}.
   * <ul>
   *   <li>Then return {@link FileResource#FileResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#as(Class)}
   */
  @Test
  public void testAs_thenReturnFileResource() throws BuildException {
    // Arrange
    FileResource other = new FileResource();

    BZip2Resource bZip2Resource = new BZip2Resource(other);
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertSame(other, bZip2Resource.as(clazz));
  }

  /**
   * Test {@link ResourceDecorator#compareTo(Resource)} with {@code Resource}.
   * <p>
   * Method under test: {@link ResourceDecorator#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource() throws BuildException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.compareTo(new Resource()));
  }

  /**
   * Test {@link ResourceDecorator#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource(ResourceCollection)} with other is {@link BZip2Resource#BZip2Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_givenBZip2ResourceWithOtherIsBZip2Resource() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.compareTo(new Resource()));
  }

  /**
   * Test {@link ResourceDecorator#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>When {@link BZip2Resource#BZip2Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_givenBZip2Resource_whenBZip2Resource() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.compareTo(new BZip2Resource()));
  }

  /**
   * Test {@link ResourceDecorator#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_givenBZip2Resource_whenResource() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.compareTo(new Resource()));
  }

  /**
   * Test {@link ResourceDecorator#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>When {@link BZip2Resource#BZip2Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_whenBZip2Resource() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.compareTo(new BZip2Resource()));
  }

  /**
   * Test {@link ResourceDecorator#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_whenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());
    Resource r = new Resource();

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.compareTo(new MappedResource(r, new CutDirsMapper())));
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new BZip2Resource(), 1);
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then throw exception.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenThrowException() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.equals(new BZip2Resource()));
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then throw exception.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenThrowException2() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource(new BZip2Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2Resource.equals(new BZip2Resource()));
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new BZip2Resource(), null);
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new BZip2Resource(), "Different type to ResourceDecorator");
  }

  /**
   * Test {@link ResourceDecorator#getResource()}.
   * <p>
   * Method under test: {@link ResourceDecorator#getResource()}
   */
  @Test
  public void testGetResource() throws BuildException {
    // Arrange
    BZip2Resource other = new BZip2Resource();

    BZip2Resource bZip2Resource = new BZip2Resource(other);
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());

    // Act and Assert
    assertSame(other, bZip2Resource.getResource());
  }

  /**
   * Test {@link ResourceDecorator#getResource()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource(ResourceCollection)} with other is {@link BZip2Resource#BZip2Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getResource()}
   */
  @Test
  public void testGetResource_givenBZip2ResourceWithOtherIsBZip2Resource() {
    // Arrange
    BZip2Resource other = new BZip2Resource();

    // Act and Assert
    assertSame(other, (new BZip2Resource(other)).getResource());
  }

  /**
   * Test {@link ResourceDecorator#getResource()}.
   * <ul>
   *   <li>Given {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceDecorator#getResource()}
   */
  @Test
  public void testGetResource_givenBZip2Resource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).getResource());
  }

  /**
   * Test {@link ResourceDecorator#setName(String)}.
   * <p>
   * Method under test: {@link ResourceDecorator#setName(String)}
   */
  @Test
  public void testSetName() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).setName("Name"));
  }

  /**
   * Test {@link ResourceDecorator#setExists(boolean)}.
   * <p>
   * Method under test: {@link ResourceDecorator#setExists(boolean)}
   */
  @Test
  public void testSetExists() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).setExists(true));
  }

  /**
   * Test {@link ResourceDecorator#setLastModified(long)}.
   * <p>
   * Method under test: {@link ResourceDecorator#setLastModified(long)}
   */
  @Test
  public void testSetLastModified() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).setLastModified(1L));
  }

  /**
   * Test {@link ResourceDecorator#setDirectory(boolean)}.
   * <p>
   * Method under test: {@link ResourceDecorator#setDirectory(boolean)}
   */
  @Test
  public void testSetDirectory() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).setDirectory(true));
  }

  /**
   * Test {@link ResourceDecorator#setSize(long)}.
   * <p>
   * Method under test: {@link ResourceDecorator#setSize(long)}
   */
  @Test
  public void testSetSize() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2Resource()).setSize(3L));
  }
}
