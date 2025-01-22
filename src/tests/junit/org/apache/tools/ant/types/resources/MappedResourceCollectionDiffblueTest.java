package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Iterator;
import java.util.Stack;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class MappedResourceCollectionDiffblueTest {
  /**
   * Test {@link MappedResourceCollection#add(ResourceCollection)} with {@code c}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#add(ResourceCollection)}
   */
  @Test
  public void testAddWithC_givenMappedResourceCollectionAddNone_thenThrowBuildException() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> mappedResourceCollection.add(Resources.NONE));
  }

  /**
   * Test {@link MappedResourceCollection#add(ResourceCollection)} with {@code c}.
   * <ul>
   *   <li>Then {@link MappedResourceCollection} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#add(ResourceCollection)}
   */
  @Test
  public void testAddWithC_thenMappedResourceCollectionSizeIsZero() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();

    // Act
    mappedResourceCollection.add(Resources.NONE);

    // Assert
    assertEquals(0, mappedResourceCollection.size());
    assertTrue(mappedResourceCollection.isEmpty());
  }

  /**
   * Test {@link MappedResourceCollection#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_thenThrowBuildException() {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> mappedResourceCollection.add(new CutDirsMapper()));
  }

  /**
   * Test {@link MappedResourceCollection#isFilesystemOnly()}.
   * <p>
   * Method under test: {@link MappedResourceCollection#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(Resources.NONE);
    Stack<Object> stk = new Stack<>();
    mappedResourceCollection.dieOnCircularReference(stk, new Project());

    // Act and Assert
    assertFalse(mappedResourceCollection.isFilesystemOnly());
  }

  /**
   * Test {@link MappedResourceCollection#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenMappedResourceCollectionAddNone_thenReturnFalse() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertFalse(mappedResourceCollection.isFilesystemOnly());
  }

  /**
   * Test {@link MappedResourceCollection#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenMappedResourceCollection_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MappedResourceCollection()).isFilesystemOnly());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenConcatAddFilelistFileList_thenReturnOne() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals(1, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code At least one resource must be provided, or some text.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenConcatAddTextAtLeastOneResourceMustBeProvidedOrSomeText() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addText("At least one resource must be provided, or some text.");

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals(1, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource(String)} with name is {@code No directory specified for %s.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenConcatDestIsResourceWithNameIsNoDirectorySpecifiedForS() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource("No directory specified for %s."));
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals(1, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenConcatDestIsResource_thenReturnOne() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals(1, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenConcatProjectIsProject_thenReturnOne() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setProject(new Project());
    c.addText("At least one resource must be provided, or some text.");

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals(1, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenFileNameNameIsName_thenReturnOne() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals(1, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenMappedResourceCollectionAddCutDirsMapper_thenReturnZero() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(new CutDirsMapper());
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenMappedResourceCollectionAddNone_thenReturnZero() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) EnableMultipleMappings is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenMappedResourceCollectionEnableMultipleMappingsIsTrue() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.setEnableMultipleMappings(true);
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenMappedResourceCollection_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MappedResourceCollection()).size());
  }

  /**
   * Test {@link MappedResourceCollection#size()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#size()}
   */
  @Test
  public void testSize_givenProjectAddBuildListenerAntClassLoader_thenReturnOne() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat c = new Concat();
    c.setProject(project);
    c.addText("At least one resource must be provided, or some text.");

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals(1, mappedResourceCollection.size());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addText("At least one resource must be provided, or some text.");

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = mappedResourceCollection.iterator();

    // Assert
    Resource nextResult = actualIteratorResult.next();
    assertTrue(nextResult instanceof MappedResource);
    Resource resource = ((MappedResource) nextResult).getResource();
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        resource.toLongString());
    assertEquals("MappedResource \"concat (At least one resource must be provided, or some text.)\"",
        nextResult.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", nextResult.getName());
    assertEquals("concat (At least one resource must be provided, or some text.)", resource.getName());
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_givenConcatDestIsResource() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = mappedResourceCollection.iterator();

    // Assert
    Resource nextResult = actualIteratorResult.next();
    assertTrue(nextResult instanceof MappedResource);
    Resource resource = ((MappedResource) nextResult).getResource();
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("MappedResource \"concat ()\"", nextResult.toLongString());
    assertEquals("concat ()", nextResult.getName());
    assertEquals("concat ()", resource.getName());
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource(String)} with name is {@code No directory specified for %s.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_givenConcatDestIsResourceWithNameIsNoDirectorySpecifiedForS() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource("No directory specified for %s."));
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = mappedResourceCollection.iterator();

    // Assert
    Resource nextResult = actualIteratorResult.next();
    assertTrue(nextResult instanceof MappedResource);
    Resource resource = ((MappedResource) nextResult).getResource();
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("MappedResource \"concat ()\"", nextResult.toLongString());
    assertEquals("concat ()", nextResult.getName());
    assertEquals("concat ()", resource.getName());
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_givenConcatProjectIsProject() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setProject(new Project());
    c.addText("At least one resource must be provided, or some text.");

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = mappedResourceCollection.iterator();

    // Assert
    Resource nextResult = actualIteratorResult.next();
    assertTrue(nextResult instanceof MappedResource);
    Resource resource = ((MappedResource) nextResult).getResource();
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        resource.toLongString());
    assertEquals("MappedResource \"concat (At least one resource must be provided, or some text.)\"",
        nextResult.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", nextResult.getName());
    assertEquals("concat (At least one resource must be provided, or some text.)", resource.getName());
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_givenFileNameNameIsName() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = mappedResourceCollection.iterator();

    // Assert
    Resource nextResult = actualIteratorResult.next();
    assertTrue(nextResult instanceof MappedResource);
    Resource resource = ((MappedResource) nextResult).getResource();
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("MappedResource \"concat ()\"", nextResult.toLongString());
    assertEquals("concat ()", nextResult.getName());
    assertEquals("concat ()", resource.getName());
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_givenMappedResourceCollectionAddCutDirsMapper_thenReturnNotHasNext() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(new CutDirsMapper());
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertFalse(mappedResourceCollection.iterator().hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_givenMappedResourceCollectionAddNone_thenReturnNotHasNext() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertFalse(mappedResourceCollection.iterator().hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) EnableMultipleMappings is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_givenMappedResourceCollectionEnableMultipleMappingsIsTrue() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.setEnableMultipleMappings(true);
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertFalse(mappedResourceCollection.iterator().hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_givenMappedResourceCollection_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MappedResourceCollection()).iterator());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat c = new Concat();
    c.setProject(project);
    c.addText("At least one resource must be provided, or some text.");

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = mappedResourceCollection.iterator();

    // Assert
    Resource nextResult = actualIteratorResult.next();
    assertTrue(nextResult instanceof MappedResource);
    Resource resource = ((MappedResource) nextResult).getResource();
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        resource.toLongString());
    assertEquals("MappedResource \"concat (At least one resource must be provided, or some text.)\"",
        nextResult.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", nextResult.getName());
    assertEquals("concat (At least one resource must be provided, or some text.)", resource.getName());
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#iterator()}.
   * <ul>
   *   <li>Then return next Resource toLongString is {@code Concat$ConcatResource "concat ()"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#iterator()}
   */
  @Test
  public void testIterator_thenReturnNextResourceToLongStringIsConcatConcatResourceConcat() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = mappedResourceCollection.iterator();

    // Assert
    Resource nextResult = actualIteratorResult.next();
    assertTrue(nextResult instanceof MappedResource);
    Resource resource = ((MappedResource) nextResult).getResource();
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("MappedResource \"concat ()\"", nextResult.toLongString());
    assertEquals("concat ()", nextResult.getName());
    assertEquals("concat ()", resource.getName());
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link MappedResourceCollection#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor).</li>
   *   <li>Then {@link MappedResourceCollection} (default constructor) Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenMappedResourceCollection_thenMappedResourceCollectionReference() {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    Reference r = new Reference("42");

    // Act
    mappedResourceCollection.setRefid(r);

    // Assert
    assertTrue(mappedResourceCollection.isReference());
    assertSame(r, mappedResourceCollection.getRefid());
  }

  /**
   * Test {@link MappedResourceCollection#createMapper()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#createMapper()}
   */
  @Test
  public void testCreateMapper_givenMappedResourceCollection_thenReturnLocationFileNameIsNull() throws BuildException {
    // Arrange and Act
    Mapper actualCreateMapperResult = (new MappedResourceCollection()).createMapper();

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
   * Test {@link MappedResourceCollection#createMapper()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#createMapper()}
   */
  @Test
  public void testCreateMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> mappedResourceCollection.createMapper());
  }

  /**
   * Test {@link MappedResourceCollection#clone()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#clone()}
   */
  @Test
  public void testClone_givenMappedResourceCollection() {
    // Arrange and Act
    Object actualCloneResult = (new MappedResourceCollection()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof MappedResourceCollection);
    Location location = ((MappedResourceCollection) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((MappedResourceCollection) actualCloneResult).getDescription());
    assertNull(((MappedResourceCollection) actualCloneResult).getProject());
    assertNull(((MappedResourceCollection) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(((MappedResourceCollection) actualCloneResult).isReference());
  }

  /**
   * Test {@link MappedResourceCollection#clone()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#clone()}
   */
  @Test
  public void testClone_givenMappedResourceCollectionAddNone_thenReturnSizeIsZero() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(Resources.NONE);

    // Act
    Object actualCloneResult = mappedResourceCollection.clone();

    // Assert
    assertTrue(actualCloneResult instanceof MappedResourceCollection);
    Location location = ((MappedResourceCollection) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((MappedResourceCollection) actualCloneResult).getDescription());
    assertNull(((MappedResourceCollection) actualCloneResult).getProject());
    assertNull(((MappedResourceCollection) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, ((MappedResourceCollection) actualCloneResult).size());
    assertFalse(((MappedResourceCollection) actualCloneResult).isReference());
    assertTrue(((MappedResourceCollection) actualCloneResult).isEmpty());
  }

  /**
   * Test {@link MappedResourceCollection#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_thenThrowBuildException() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(new CutDirsMapper());
    Stack<Object> stk = new Stack<>();

    // Act and Assert
    assertThrows(BuildException.class, () -> mappedResourceCollection.dieOnCircularReference(stk, new Project()));
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenConcatAddFilelistFileList_thenReturnConcat() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals("concat ()", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource(String)} with name is {@code No directory specified for %s.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenConcatDestIsResourceWithNameIsNoDirectorySpecifiedForS() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource("No directory specified for %s."));
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals("concat ()", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenConcatDestIsResource_thenReturnConcat() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals("concat ()", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenConcatProjectIsProject() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setProject(new Project());
    c.addText("At least one resource must be provided, or some text.");

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals("concat (At least one resource must be provided, or some text.)", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return {@code concat (}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenConcatResourceNameIsConcat_thenReturnConcat() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setResourceName("concat (");
    c.addFilelist(new FileList());

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals("concat (", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code concat (}.</li>
   *   <li>Then return {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenFileNameNameIsConcat_thenReturnConcat() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals("concat ()", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenMappedResourceCollectionAddNone_thenReturnEmptyString() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertEquals("", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenMappedResourceCollectionAddNone_thenReturnEmptyString2() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(new CutDirsMapper());
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertEquals("", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link MappedResourceCollection} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenMappedResourceCollection_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MappedResourceCollection()).toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat c = new Concat();
    c.setProject(project);
    c.addText("At least one resource must be provided, or some text.");

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals("concat (At least one resource must be provided, or some text.)", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Then return {@code concat (At least one resource must be provided, or some text.)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_thenReturnConcatAtLeastOneResourceMustBeProvidedOrSomeText() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addText("At least one resource must be provided, or some text.");

    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.add(c);

    // Act and Assert
    assertEquals("concat (At least one resource must be provided, or some text.)", mappedResourceCollection.toString());
  }

  /**
   * Test {@link MappedResourceCollection#toString()}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResourceCollection#toString()}
   */
  @Test
  public void testToString_thenReturnEmptyString() throws BuildException {
    // Arrange
    MappedResourceCollection mappedResourceCollection = new MappedResourceCollection();
    mappedResourceCollection.setEnableMultipleMappings(true);
    mappedResourceCollection.add(Resources.NONE);

    // Act and Assert
    assertEquals("", mappedResourceCollection.toString());
  }

  /**
   * Test new {@link MappedResourceCollection} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MappedResourceCollection}
   */
  @Test
  public void testNewMappedResourceCollection() {
    // Arrange and Act
    MappedResourceCollection actualMappedResourceCollection = new MappedResourceCollection();

    // Assert
    Location location = actualMappedResourceCollection.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMappedResourceCollection.getDescription());
    assertNull(actualMappedResourceCollection.getProject());
    assertNull(actualMappedResourceCollection.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMappedResourceCollection.isReference());
  }
}
