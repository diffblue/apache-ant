package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.junit.Test;

public class UnionDiffblueTest {
  /**
   * Test {@link Union#getInstance(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then return ResourceCollections first is {@link Resources#NONE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getInstance(ResourceCollection)}
   */
  @Test
  public void testGetInstance_whenNone_thenReturnResourceCollectionsFirstIsNone() {
    // Arrange
    ResourceCollection rc = Resources.NONE;

    // Act and Assert
    List<ResourceCollection> resourceCollections = Union.getInstance(rc).getResourceCollections();
    assertEquals(1, resourceCollections.size());
    assertSame(rc, resourceCollections.get(0));
  }

  /**
   * Test {@link Union#getInstance(ResourceCollection)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then AllToStrings return {@link Set}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getInstance(ResourceCollection)}
   */
  @Test
  public void testGetInstance_whenNull_thenAllToStringsReturnSet() {
    // Arrange and Act
    Union actualInstance = Union.getInstance(null);

    // Assert
    Collection<String> allToStrings = actualInstance.getAllToStrings();
    assertTrue(allToStrings instanceof Set);
    Collection<Resource> collection = actualInstance.getCollection();
    assertTrue(collection instanceof Set);
    assertNull(actualInstance.getProject());
    assertEquals(0, actualInstance.size());
    assertTrue(allToStrings.isEmpty());
    assertTrue(collection.isEmpty());
    assertTrue(actualInstance.getResourceCollections().isEmpty());
    assertTrue(actualInstance.getAllResources().isEmpty());
    assertTrue(actualInstance.isEmpty());
  }

  /**
   * Test {@link Union#getInstance(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#systemBootClasspath}.</li>
   *   <li>Then return ResourceCollections size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getInstance(ResourceCollection)}
   */
  @Test
  public void testGetInstance_whenSystemBootClasspath_thenReturnResourceCollectionsSizeIsOne() {
    // Arrange, Act and Assert
    assertEquals(1, Union.getInstance(Path.systemBootClasspath).getResourceCollections().size());
  }

  /**
   * Test {@link Union#getInstance(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Union#Union()}.</li>
   *   <li>Then AllToStrings return {@link Set}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getInstance(ResourceCollection)}
   */
  @Test
  public void testGetInstance_whenUnion_thenAllToStringsReturnSet() {
    // Arrange and Act
    Union actualInstance = Union.getInstance(new Union());

    // Assert
    Collection<String> allToStrings = actualInstance.getAllToStrings();
    assertTrue(allToStrings instanceof Set);
    Collection<Resource> collection = actualInstance.getCollection();
    assertTrue(collection instanceof Set);
    assertNull(actualInstance.getProject());
    assertEquals(0, actualInstance.size());
    assertTrue(allToStrings.isEmpty());
    assertTrue(collection.isEmpty());
    assertTrue(actualInstance.getResourceCollections().isEmpty());
    assertTrue(actualInstance.getAllResources().isEmpty());
    assertTrue(actualInstance.isEmpty());
  }

  /**
   * Test {@link Union#Union()}.
   * <p>
   * Method under test: {@link Union#Union()}
   */
  @Test
  public void testNewUnion() {
    // Arrange and Act
    Union actualUnion = new Union();

    // Assert
    Location location = actualUnion.getLocation();
    assertNull(location.getFileName());
    assertNull(actualUnion.getDescription());
    assertNull(actualUnion.getProject());
    assertNull(actualUnion.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(actualUnion.isCache());
  }

  /**
   * Test {@link Union#Union(Project)}.
   * <p>
   * Method under test: {@link Union#Union(Project)}
   */
  @Test
  public void testNewUnion2() {
    // Arrange
    Project project = new Project();

    // Act
    Union actualUnion = new Union(project);

    // Assert
    Collection<String> allToStrings = actualUnion.getAllToStrings();
    assertTrue(allToStrings instanceof Set);
    Collection<Resource> collection = actualUnion.getCollection();
    assertTrue(collection instanceof Set);
    assertNull(actualUnion.getDescription());
    assertNull(actualUnion.getRefid());
    assertEquals(0, actualUnion.size());
    assertFalse(actualUnion.isReference());
    assertTrue(allToStrings.isEmpty());
    assertTrue(collection.isEmpty());
    assertTrue(actualUnion.getResourceCollections().isEmpty());
    assertTrue(actualUnion.getAllResources().isEmpty());
    assertTrue(actualUnion.isEmpty());
    assertTrue(actualUnion.isCache());
    assertSame(project, actualUnion.getProject());
  }

  /**
   * Test {@link Union#Union(Project, ResourceCollection)}.
   * <ul>
   *   <li>When {@link AllButFirst} (default constructor).</li>
   *   <li>Then ResourceCollections first return {@link AllButFirst}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#Union(Project, ResourceCollection)}
   */
  @Test
  public void testNewUnion_whenAllButFirst_thenResourceCollectionsFirstReturnAllButFirst() {
    // Arrange
    Project project = new Project();
    AllButFirst rc = new AllButFirst();

    // Act and Assert
    List<ResourceCollection> resourceCollections = (new Union(project, rc)).getResourceCollections();
    assertEquals(1, resourceCollections.size());
    ResourceCollection getResult = resourceCollections.get(0);
    assertTrue(getResult instanceof AllButFirst);
    assertSame(project, rc.getProject());
    assertSame(rc, getResult);
  }

  /**
   * Test {@link Union#Union(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then return ResourceCollections first is {@link Resources#NONE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#Union(ResourceCollection)}
   */
  @Test
  public void testNewUnion_whenNone_thenReturnResourceCollectionsFirstIsNone() {
    // Arrange
    ResourceCollection rc = Resources.NONE;

    // Act
    Union actualUnion = new Union(rc);

    // Assert
    Collection<String> allToStrings = actualUnion.getAllToStrings();
    assertTrue(allToStrings instanceof Set);
    Collection<Resource> collection = actualUnion.getCollection();
    assertTrue(collection instanceof Set);
    assertNull(actualUnion.getProject());
    assertEquals(0, actualUnion.size());
    List<ResourceCollection> resourceCollections = actualUnion.getResourceCollections();
    assertEquals(1, resourceCollections.size());
    assertTrue(allToStrings.isEmpty());
    assertTrue(collection.isEmpty());
    assertTrue(actualUnion.getAllResources().isEmpty());
    assertTrue(actualUnion.isEmpty());
    assertSame(rc, resourceCollections.get(0));
  }

  /**
   * Test {@link Union#Union(Project, ResourceCollection)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#Union(Project, ResourceCollection)}
   */
  @Test
  public void testNewUnion_whenNull_thenReturnProjectIsNull() {
    // Arrange
    ResourceCollection rc = Resources.NONE;

    // Act
    Union actualUnion = new Union(null, rc);

    // Assert
    assertNull(actualUnion.getProject());
    List<ResourceCollection> resourceCollections = actualUnion.getResourceCollections();
    assertEquals(1, resourceCollections.size());
    assertSame(rc, resourceCollections.get(0));
  }

  /**
   * Test {@link Union#Union(Project, ResourceCollection)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return ResourceCollections Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#Union(Project, ResourceCollection)}
   */
  @Test
  public void testNewUnion_whenNull_thenReturnResourceCollectionsEmpty() {
    // Arrange and Act
    Union actualUnion = new Union(new Project(), null);

    // Assert
    List<ResourceCollection> resourceCollections = actualUnion.getResourceCollections();
    assertTrue(resourceCollections.isEmpty());
    assertEquals(resourceCollections, actualUnion.getProject().getBuildListeners());
  }

  /**
   * Test {@link Union#Union(ResourceCollection)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return ResourceCollections Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#Union(ResourceCollection)}
   */
  @Test
  public void testNewUnion_whenNull_thenReturnResourceCollectionsEmpty2() {
    // Arrange and Act
    Union actualUnion = new Union((ResourceCollection) null);

    // Assert
    Collection<String> allToStrings = actualUnion.getAllToStrings();
    assertTrue(allToStrings instanceof Set);
    Collection<Resource> collection = actualUnion.getCollection();
    assertTrue(collection instanceof Set);
    assertNull(actualUnion.getProject());
    assertEquals(0, actualUnion.size());
    assertTrue(allToStrings.isEmpty());
    assertTrue(collection.isEmpty());
    assertTrue(actualUnion.getResourceCollections().isEmpty());
    assertTrue(actualUnion.getAllResources().isEmpty());
    assertTrue(actualUnion.isEmpty());
  }

  /**
   * Test {@link Union#Union(Project, ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#systemBootClasspath}.</li>
   *   <li>Then ResourceCollections first return {@link Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#Union(Project, ResourceCollection)}
   */
  @Test
  public void testNewUnion_whenSystemBootClasspath_thenResourceCollectionsFirstReturnPath() {
    // Arrange
    Path rc = Path.systemBootClasspath;

    // Act and Assert
    List<ResourceCollection> resourceCollections = (new Union(new Project(), rc)).getResourceCollections();
    assertEquals(1, resourceCollections.size());
    ResourceCollection getResult = resourceCollections.get(0);
    assertTrue(getResult instanceof Path);
    assertSame(rc.systemBootClasspath, getResult);
  }

  /**
   * Test {@link Union#Union(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#systemBootClasspath}.</li>
   *   <li>Then return ResourceCollections size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#Union(ResourceCollection)}
   */
  @Test
  public void testNewUnion_whenSystemBootClasspath_thenReturnResourceCollectionsSizeIsOne() {
    // Arrange, Act and Assert
    assertEquals(1, (new Union(Path.systemBootClasspath)).getResourceCollections().size());
  }

  /**
   * Test {@link Union#list()}.
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList() {
    // Arrange
    Concat rc = new Concat();
    rc.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertArrayEquals(new String[]{"concat (At least one resource must be provided, or some text.)"},
        Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList2() {
    // Arrange
    FileName name = new FileName();
    name.setName("..");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act
    String[] actualListResult = Union.getInstance(rc).list();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("user.home"), "Downloads").toString()},
        actualListResult);
  }

  /**
   * Test {@link Union#list()}.
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList3() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileName name2 = new FileName();
    name2.setName(".");

    FileList rc = new FileList();
    rc.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    rc.addConfiguredFile(name2);
    rc.addConfiguredFile(name);

    // Act
    String[] actualListResult = Union.getInstance(rc).list();

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()},
        actualListResult);
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return array of {@link String} with {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenConcatAddFilelistFileList_thenReturnArrayOfStringWithConcat() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    // Act and Assert
    assertArrayEquals(new String[]{"concat ()"}, Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return array of {@link String} with {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenConcatDestIsResource_thenReturnArrayOfStringWithConcat() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    // Act and Assert
    assertArrayEquals(new String[]{"concat ()"}, Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenConcatProjectIsProject() {
    // Arrange
    Concat rc = new Concat();
    rc.setProject(new Project());
    rc.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertArrayEquals(new String[]{"concat (At least one resource must be provided, or some text.)"},
        Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return array of {@link String} with {@code concat (}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenConcatResourceNameIsConcat_thenReturnArrayOfStringWithConcat() {
    // Arrange
    Concat rc = new Concat();
    rc.setResourceName("concat (");
    rc.addFilelist(new FileList());

    // Act and Assert
    assertArrayEquals(new String[]{"concat ("}, Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code concat (}.</li>
   *   <li>Then return array of {@link String} with {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenFileNameNameIsConcat_thenReturnArrayOfStringWithConcat() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act and Assert
    assertArrayEquals(new String[]{"concat ()"}, Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then return array of {@link String} with Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenFileNameNameIsDot_thenReturnArrayOfStringWithPropertyIsUserDir() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileName name2 = new FileName();
    name2.setName(".");

    FileList rc = new FileList();
    rc.addConfiguredFile(name2);
    rc.addConfiguredFile(name);

    // Act and Assert
    assertArrayEquals(new String[]{System.getProperty("user.dir")}, Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return array of {@link String} with {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenFileNameNameIsUsers_thenReturnArrayOfStringWithConcat() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act and Assert
    assertArrayEquals(new String[]{"concat ()"}, Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given Instance is {@link Archives} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenInstanceIsArchives_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Union.getInstance(new Archives()).list().length);
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given Instance is {@link FileList#FileList()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenInstanceIsFileList_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Union.getInstance(new FileList()).list().length);
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given Instance is {@link Resources#NONE}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenInstanceIsNone_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Union.getInstance(Resources.NONE).list().length);
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertArrayEquals(new String[]{"concat (At least one resource must be provided, or some text.)"},
        Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertArrayEquals(new String[]{"concat (At least one resource must be provided, or some text.)"},
        Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Given {@link Union#Union()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_givenUnion_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Union()).list().length);
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Then return array of {@link String} with Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_thenReturnArrayOfStringWithPropertyIsUserDir() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act and Assert
    assertArrayEquals(new String[]{System.getProperty("user.dir")}, Union.getInstance(rc).list());
  }

  /**
   * Test {@link Union#list()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code (unbound file resource)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#list()}
   */
  @Test
  public void testList_thenReturnArrayOfStringWithUnboundFileResource() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"(unbound file resource)"}, Union.getInstance(new FileResource()).list());
  }

  /**
   * Test {@link Union#listResources()}.
   * <ul>
   *   <li>Given Instance is {@link Archives} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#listResources()}
   */
  @Test
  public void testListResources_givenInstanceIsArchives_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Union.getInstance(new Archives()).listResources().length);
  }

  /**
   * Test {@link Union#listResources()}.
   * <ul>
   *   <li>Given Instance is {@link FileList#FileList()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#listResources()}
   */
  @Test
  public void testListResources_givenInstanceIsFileList_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Union.getInstance(new FileList()).listResources().length);
  }

  /**
   * Test {@link Union#listResources()}.
   * <ul>
   *   <li>Given Instance is {@link Resources#NONE}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#listResources()}
   */
  @Test
  public void testListResources_givenInstanceIsNone_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Union.getInstance(Resources.NONE).listResources().length);
  }

  /**
   * Test {@link Union#listResources()}.
   * <ul>
   *   <li>Given {@link Union#Union()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#listResources()}
   */
  @Test
  public void testListResources_givenUnion_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Union()).listResources().length);
  }

  /**
   * Test {@link Union#listResources()}.
   * <ul>
   *   <li>Then return first element is {@link FileResource#FileResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#listResources()}
   */
  @Test
  public void testListResources_thenReturnFirstElementIsFileResource() {
    // Arrange
    FileResource rc = new FileResource();

    // Act
    Resource[] actualListResourcesResult = Union.getInstance(rc).listResources();

    // Assert
    assertEquals(1, actualListResourcesResult.length);
    assertSame(rc, actualListResourcesResult[0]);
  }

  /**
   * Test {@link Union#getCollection()}.
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileName name2 = new FileName();
    name2.setName(".");

    FileList rc = new FileList();
    rc.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    rc.addConfiguredFile(name2);
    rc.addConfiguredFile(name);

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean() {
    // Arrange
    Concat rc = new Concat();
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean2() {
    // Arrange
    Concat rc = new Concat();
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(false);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean3() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    rc.addFilelist(new FileList());

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenConcatAddFilelistFileList_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>When {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenConcatAddFilelistFileList_whenFalse() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(false);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link FileResource#FileResource()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenConcatDestIsFileResource_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new FileResource());
    rc.addFilelist(new FileList());

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenConcatDestIsResource_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenConcatProjectIsProject_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setProject(new Project());
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenConcatResourceNameIsConcat_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setResourceName("concat (");
    rc.addFilelist(new FileList());

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>When {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenConcatResourceNameIsConcat_whenFalse() {
    // Arrange
    Concat rc = new Concat();
    rc.setResourceName("concat (");
    rc.addFilelist(new FileList());

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(false);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code concat (}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenFileNameNameIsConcat_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code ..}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenFileNameNameIsDotDot_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("..");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code .}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenFileNameNameIsDot_whenTrue_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileName name2 = new FileName();
    name2.setName(".");

    FileList rc = new FileList();
    rc.addConfiguredFile(name2);
    rc.addConfiguredFile(name);

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is empty string.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenFileNameNameIsEmptyString_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenFileNameNameIsUsers_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given Instance is {@link Archives} (default constructor).</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenInstanceIsArchives_whenTrue_thenReturnEmpty() {
    // Arrange and Act
    Collection<Object> actualCollection = Union.getInstance(new Archives()).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given Instance is {@link FileList#FileList()}.</li>
   *   <li>When {@code false}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenInstanceIsFileList_whenFalse_thenReturnEmpty() {
    // Arrange and Act
    Collection<Object> actualCollection = Union.getInstance(new FileList()).getCollection(false);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given Instance is {@link FileList#FileList()}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenInstanceIsFileList_whenTrue_thenReturnEmpty() {
    // Arrange and Act
    Collection<Object> actualCollection = Union.getInstance(new FileList()).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given Instance is {@link FileResource#FileResource()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenInstanceIsFileResource_thenReturnSizeIsOne() {
    // Arrange and Act
    Collection<Object> actualCollection = Union.getInstance(new FileResource()).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given Instance is {@link Resources#NONE}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenInstanceIsNone_whenTrue_thenReturnEmpty() {
    // Arrange and Act
    Collection<Object> actualCollection = Union.getInstance(Resources.NONE).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenJavaLangObject_whenTrue_thenReturnSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<Object> actualCollection = Union.getInstance(rc).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Union#Union()}.</li>
   *   <li>When {@code false}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenUnion_whenFalse_thenReturnEmpty() {
    // Arrange and Act
    Collection<Object> actualCollection = (new Union()).getCollection(false);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getCollection(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link Union#Union()}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection(boolean)}
   */
  @Test
  public void testGetCollectionWithBoolean_givenUnion_whenTrue_thenReturnEmpty() {
    // Arrange and Act
    Collection<Object> actualCollection = (new Union()).getCollection(true);

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatAddFilelistFileList_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code At least one resource must be provided, or some text.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatAddTextAtLeastOneResourceMustBeProvidedOrSomeText() {
    // Arrange
    Concat rc = new Concat();
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatDestIsResource_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatProjectIsProject_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setProject(new Project());
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatResourceNameIsConcat_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setResourceName("concat (");
    rc.addFilelist(new FileList());

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code concat (}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenFileNameNameIsConcat_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code ..}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenFileNameNameIsDotDot_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("..");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenFileNameNameIsDot_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileName name2 = new FileName();
    name2.setName(".");

    FileList rc = new FileList();
    rc.addConfiguredFile(name2);
    rc.addConfiguredFile(name);

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is empty string.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenFileNameNameIsEmptyString_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenFileNameNameIsUsers_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given Instance is {@link Archives} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenInstanceIsArchives_thenReturnEmpty() {
    // Arrange and Act
    Collection<Resource> actualCollection = Union.getInstance(new Archives()).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given Instance is {@link FileList#FileList()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenInstanceIsFileList_thenReturnEmpty() {
    // Arrange and Act
    Collection<Resource> actualCollection = Union.getInstance(new FileList()).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given Instance is {@link FileResource#FileResource()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenInstanceIsFileResource_thenReturnSizeIsOne() {
    // Arrange and Act
    Collection<Resource> actualCollection = Union.getInstance(new FileResource()).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given Instance is {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenInstanceIsNone_thenReturnEmpty() {
    // Arrange and Act
    Collection<Resource> actualCollection = Union.getInstance(Resources.NONE).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenJavaLangObject_thenReturnSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenProjectAddBuildListenerAntClassLoader_thenReturnSizeIsOne() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<Resource> actualCollection = Union.getInstance(rc).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Union#getCollection()}.
   * <ul>
   *   <li>Given {@link Union#Union()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getCollection()}
   */
  @Test
  public void testGetCollection_givenUnion_thenReturnEmpty() {
    // Arrange and Act
    Collection<Resource> actualCollection = (new Union()).getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings() {
    // Arrange
    Concat rc = new Concat();
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat (At least one resource must be provided, or some text.)"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings2() {
    // Arrange
    FileName name = new FileName();
    name.setName("..");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains(Paths.get(System.getProperty("user.home"), "Downloads").toString()));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings3() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    rc.addFilelist(new FileList());

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat ()"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return contains {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenConcatAddFilelistFileList_thenReturnContainsConcat() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat ()"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link FileResource#FileResource()}.</li>
   *   <li>Then return contains {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenConcatDestIsFileResource_thenReturnContainsConcat() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new FileResource());
    rc.addFilelist(new FileList());

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat ()"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource(String)} with name is {@code No directory specified for %s.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenConcatDestIsResourceWithNameIsNoDirectorySpecifiedForS() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource("No directory specified for %s."));
    rc.addFilelist(new FileList());

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat ()"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return contains {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenConcatDestIsResource_thenReturnContainsConcat() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat ()"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenConcatProjectIsProject() {
    // Arrange
    Concat rc = new Concat();
    rc.setProject(new Project());
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat (At least one resource must be provided, or some text.)"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return contains {@code concat (}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenConcatResourceNameIsConcat_thenReturnContainsConcat() {
    // Arrange
    Concat rc = new Concat();
    rc.setResourceName("concat (");
    rc.addFilelist(new FileList());

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat ("));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code concat (}.</li>
   *   <li>Then return contains {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenFileNameNameIsConcat_thenReturnContainsConcat() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat ()"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then return contains Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenFileNameNameIsDot_thenReturnContainsPropertyIsUserDir() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileName name2 = new FileName();
    name2.setName(".");

    FileList rc = new FileList();
    rc.addConfiguredFile(name2);
    rc.addConfiguredFile(name);

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains(System.getProperty("user.dir")));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return contains {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenFileNameNameIsUsers_thenReturnContainsConcat() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat ()"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given Instance is {@link Archives} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenInstanceIsArchives_thenReturnEmpty() {
    // Arrange and Act
    Collection<String> actualAllToStrings = Union.getInstance(new Archives()).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertTrue(actualAllToStrings.isEmpty());
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given Instance is {@link FileList#FileList()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenInstanceIsFileList_thenReturnEmpty() {
    // Arrange and Act
    Collection<String> actualAllToStrings = Union.getInstance(new FileList()).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertTrue(actualAllToStrings.isEmpty());
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given Instance is {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenInstanceIsNone_thenReturnEmpty() {
    // Arrange and Act
    Collection<String> actualAllToStrings = Union.getInstance(Resources.NONE).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertTrue(actualAllToStrings.isEmpty());
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat (At least one resource must be provided, or some text.)"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("concat (At least one resource must be provided, or some text.)"));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Given {@link Union#Union()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_givenUnion_thenReturnEmpty() {
    // Arrange and Act
    Collection<String> actualAllToStrings = (new Union()).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertTrue(actualAllToStrings.isEmpty());
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Then return contains Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_thenReturnContainsPropertyIsUserDir() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act
    Collection<String> actualAllToStrings = Union.getInstance(rc).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains(System.getProperty("user.dir")));
  }

  /**
   * Test {@link Union#getAllToStrings()}.
   * <ul>
   *   <li>Then return contains {@code (unbound file resource)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllToStrings()}
   */
  @Test
  public void testGetAllToStrings_thenReturnContainsUnboundFileResource() {
    // Arrange and Act
    Collection<String> actualAllToStrings = Union.getInstance(new FileResource()).getAllToStrings();

    // Assert
    assertTrue(actualAllToStrings instanceof Set);
    assertEquals(1, actualAllToStrings.size());
    assertTrue(actualAllToStrings.contains("(unbound file resource)"));
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileName name2 = new FileName();
    name2.setName(".");

    FileList rc = new FileList();
    rc.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    rc.addConfiguredFile(name2);
    rc.addConfiguredFile(name);

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenConcatAddFilelistFileList_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code At least one resource must be provided, or some text.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenConcatAddTextAtLeastOneResourceMustBeProvidedOrSomeText() {
    // Arrange
    Concat rc = new Concat();
    rc.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenConcatDestIsResource_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenConcatProjectIsProject_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setProject(new Project());
    rc.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenConcatResourceNameIsConcat_thenReturnSizeIsOne() {
    // Arrange
    Concat rc = new Concat();
    rc.setResourceName("concat (");
    rc.addFilelist(new FileList());

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code concat (}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenFileNameNameIsConcat_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code ..}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenFileNameNameIsDotDot_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("..");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenFileNameNameIsDot_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileName name2 = new FileName();
    name2.setName(".");

    FileList rc = new FileList();
    rc.addConfiguredFile(name2);
    rc.addConfiguredFile(name);

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is empty string.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenFileNameNameIsEmptyString_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList rc = new FileList();
    rc.addConfiguredFile(name);

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenFileNameNameIsUsers_thenReturnSizeIsOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given Instance is {@link Archives} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenInstanceIsArchives_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue(Union.getInstance(new Archives()).getAllResources().isEmpty());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given Instance is {@link FileList#FileList()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenInstanceIsFileList_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue(Union.getInstance(new FileList()).getAllResources().isEmpty());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given Instance is {@link FileResource#FileResource()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenInstanceIsFileResource_thenReturnSizeIsOne() {
    // Arrange, Act and Assert
    assertEquals(1, Union.getInstance(new FileResource()).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given Instance is {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenInstanceIsNone_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue(Union.getInstance(Resources.NONE).getAllResources().isEmpty());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenJavaLangObject_thenReturnSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertEquals(1, Union.getInstance(rc).getAllResources().size());
  }

  /**
   * Test {@link Union#getAllResources()}.
   * <ul>
   *   <li>Given {@link Union#Union()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Union#getAllResources()}
   */
  @Test
  public void testGetAllResources_givenUnion_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new Union()).getAllResources().isEmpty());
  }
}
