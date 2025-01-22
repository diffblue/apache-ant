package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import javax.imageio.metadata.IIOMetadataNode;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.XMLCatalog;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;
import org.w3c.dom.Node;
import org.xml.sax.EntityResolver;

public class XmlPropertyDiffblueTest {
  /**
   * Test {@link XmlProperty#processNode(Node, String, Object)}.
   * <ul>
   *   <li>Given {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#processNode(Node, String, Object)}
   */
  @Test
  public void testProcessNode_givenIIOMetadataNodeWithFoo() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    IIOMetadataNode node = new IIOMetadataNode("foo");
    IIOMetadataNode iioMetadataNode = new IIOMetadataNode("foo");
    node.insertBefore(iioMetadataNode, new IIOMetadataNode("foo"));
    IIOMetadataNode iioMetadataNode2 = new IIOMetadataNode(":");
    node.insertBefore(iioMetadataNode2, new IIOMetadataNode(":"));

    // Act and Assert
    assertNull(xmlProperty.processNode(node, "Prefix", "Container"));
  }

  /**
   * Test {@link XmlProperty#processNode(Node, String, Object)}.
   * <ul>
   *   <li>When {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code foo} appendChild {@link IIOMetadataNode#IIOMetadataNode(String)} with {@code :}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#processNode(Node, String, Object)}
   */
  @Test
  public void testProcessNode_whenIIOMetadataNodeWithFooAppendChildIIOMetadataNodeWithColon() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    IIOMetadataNode node = new IIOMetadataNode("foo");
    node.appendChild(new IIOMetadataNode(":"));

    // Act and Assert
    assertNull(xmlProperty.processNode(node, "Prefix", "Container"));
  }

  /**
   * Test {@link XmlProperty#setFile(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setFile(File)}
   */
  @Test
  public void testSetFile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenThrowBuildException() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> xmlProperty.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    FileResource src = new FileResource(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    xmlProperty.setSrcResource(src);

    // Assert
    assertSame(src, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource2() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    Resource r = new Resource();
    MappedResource src = new MappedResource(r, new CutDirsMapper());

    // Act
    xmlProperty.setSrcResource(src);

    // Assert
    assertNull(xmlProperty.getFile());
    assertSame(src, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenDotDot_whenFileResourceNameIsDotDot() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileResource src = new FileResource();
    src.setName("..");

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.setSrcResource(src));
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenDotDot_whenFileResourceNameIsDotDot2() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileResource src = new FileResource();
    src.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    src.setName("..");

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.setSrcResource(src));
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenDot_whenFileResourceNameIsDot() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileResource src = new FileResource();
    src.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    src.setName(".");

    // Act
    xmlProperty.setSrcResource(src);

    // Assert
    assertSame(src, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenDot_whenFileResourceNameIsDot_thenThrowBuildException() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileResource src = new FileResource();
    src.setName(".");

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.setSrcResource(src));
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileResource src = new FileResource();
    src.setName("");

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.setSrcResource(src));
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenFileAttributeIsNull() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileResource src = new FileResource();
    src.setName("file attribute is null!");

    // Act
    xmlProperty.setSrcResource(src);

    // Assert
    assertSame(src, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code /NULL_FILE}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code /NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenNullFile_whenFileResourceNameIsNullFile() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileResource src = new FileResource();
    src.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    src.setName("/NULL_FILE");

    // Act
    xmlProperty.setSrcResource(src);

    // Assert
    assertSame(src, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenTrue_whenResourceDirectoryIsTrue_thenThrowBuildException() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    Resource src = new Resource();
    src.setDirectory(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.setSrcResource(src));
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>Then {@link XmlProperty} (default constructor) Resource is {@link Resource#Resource(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_thenXmlPropertyResourceIsResourceWithNameIsAttribute_name() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    Resource src = new Resource(Manifest.ATTRIBUTE_NAME);

    // Act
    xmlProperty.setSrcResource(src);

    // Assert
    assertNull(xmlProperty.getFile());
    assertSame(src, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then {@link XmlProperty} (default constructor) Resource is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenResource_thenXmlPropertyResourceIsResource() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    Resource src = new Resource();

    // Act
    xmlProperty.setSrcResource(src);

    // Assert
    assertNull(xmlProperty.getFile());
    assertSame(src, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    Concat a = new Concat();
    a.addFilelist(new FileList());

    // Act
    xmlProperty.addConfigured(a);

    // Assert
    Resource resource = xmlProperty.getResource();
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
    assertEquals(-1L, resource.getSize());
    assertEquals(0L, resource.getLastModified());
    assertFalse(resource.isFilesystemOnly());
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured2() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    Concat a = new Concat();
    a.addText("At least one resource must be provided, or some text.");

    // Act
    xmlProperty.addConfigured(a);

    // Assert
    Resource resource = xmlProperty.getResource();
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        resource.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", resource.getName());
    assertEquals(-1L, resource.getSize());
    assertEquals(0L, resource.getLastModified());
    assertFalse(resource.isFilesystemOnly());
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileList_whenPathWithProjectIsProjectAddFilelistFileList() throws BuildException {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    Path a = new Path(new Project());
    a.addFilelist(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.addConfigured(a));
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsDotDot() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileName name = new FileName();
    name.setName("..");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.addConfigured(a));
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code .}.</li>
   *   <li>When {@link FileList#FileList()} addConfiguredFile {@link FileName} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsDot_whenFileListAddConfiguredFileFileName() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileName name = new FileName();
    name.setName(".");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.addConfigured(a));
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsEmptyString() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileName name = new FileName();
    name.setName("");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.addConfigured(a));
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenProject_whenConcatProjectIsProject() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    Concat a = new Concat();
    a.setProject(new Project());
    a.addText("At least one resource must be provided, or some text.");

    // Act
    xmlProperty.addConfigured(a);

    // Assert
    Resource resource = xmlProperty.getResource();
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        resource.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", resource.getName());
    assertEquals(-1L, resource.getSize());
    assertEquals(0L, resource.getLastModified());
    assertFalse(resource.isFilesystemOnly());
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenResource_whenConcatDestIsResource() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    Concat a = new Concat();
    a.setDest(new Resource());
    a.addFilelist(new FileList());

    // Act
    xmlProperty.addConfigured(a);

    // Assert
    Resource resource = xmlProperty.getResource();
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
    assertEquals(-1L, resource.getSize());
    assertEquals(0L, resource.getLastModified());
    assertFalse(resource.isFilesystemOnly());
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link XmlProperty} (default constructor).</li>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenXmlProperty_whenFileList_thenThrowBuildException() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.addConfigured(new FileList()));
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link XmlProperty} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenXmlProperty_whenNone_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new XmlProperty()).addConfigured(Resources.NONE));
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Then {@link XmlProperty} (default constructor) File Name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_thenXmlPropertyFileNameIsAttribute_name() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    FileName name = new FileName();
    name.setName(Manifest.ATTRIBUTE_NAME);

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act
    xmlProperty.addConfigured(a);

    // Assert
    assertTrue(xmlProperty.getResource() instanceof FileResource);
    assertEquals(Manifest.ATTRIBUTE_NAME, xmlProperty.getFile().getName());
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Then {@link XmlProperty} (default constructor) Resource is {@link Resource#Resource(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_thenXmlPropertyResourceIsResourceWithNameIsAttribute_name() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    Resource a = new Resource(Manifest.ATTRIBUTE_NAME);

    // Act
    xmlProperty.addConfigured(a);

    // Assert
    assertNull(xmlProperty.getFile());
    assertSame(a, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   *   <li>Then {@link XmlProperty} (default constructor) File Name is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenPathWithPIsProjectAndPath_thenXmlPropertyFileNameIsPath() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    Project p = new Project();

    // Act
    xmlProperty.addConfigured(new Path(p, "Path"));

    // Assert
    Resource resource = xmlProperty.getResource();
    assertTrue(resource instanceof FileResource);
    assertEquals("Path", xmlProperty.getFile().getName());
    assertEquals("Path", resource.getName());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("user.dir"), "Path").toString(), "\"");
    assertEquals(expectedToLongStringResult, resource.toLongString());
    assertSame(p, resource.getProject());
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenPathWithProjectIsProject_thenThrowBuildException() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlProperty.addConfigured(new Path(new Project())));
  }

  /**
   * Test {@link XmlProperty#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then {@link XmlProperty} (default constructor) Resource is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenResource_thenXmlPropertyResourceIsResource() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    Resource a = new Resource();

    // Act
    xmlProperty.addConfigured(a);

    // Assert
    assertNull(xmlProperty.getFile());
    assertSame(a, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#setPrefix(String)}.
   * <p>
   * Method under test: {@link XmlProperty#setPrefix(String)}
   */
  @Test
  public void testSetPrefix() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    // Act
    xmlProperty.setPrefix("Prefix");

    // Assert
    assertEquals("Prefix", xmlProperty.getPrefix());
  }

  /**
   * Test {@link XmlProperty#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link XMLCatalog} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_givenProject_whenXMLCatalogProjectIsProject() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    XMLCatalog catalog = new XMLCatalog();
    catalog.setProject(new Project());
    catalog.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xmlProperty.addConfiguredXMLCatalog(catalog);

    // Assert
    EntityResolver entityResolver = xmlProperty.getEntityResolver();
    assertTrue(entityResolver instanceof XMLCatalog);
    Path catalogPath = ((XMLCatalog) entityResolver).getCatalogPath();
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XmlProperty#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_givenXMLCatalog() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    XMLCatalog catalog = new XMLCatalog();
    catalog.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xmlProperty.addConfiguredXMLCatalog(catalog);

    // Assert
    EntityResolver entityResolver = xmlProperty.getEntityResolver();
    assertTrue(entityResolver instanceof XMLCatalog);
    Path catalogPath = ((XMLCatalog) entityResolver).getCatalogPath();
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XmlProperty#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>Given {@link XmlProperty} (default constructor) addConfiguredXMLCatalog {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_givenXmlPropertyAddConfiguredXMLCatalogXMLCatalog() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    xmlProperty.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xmlProperty.addConfiguredXMLCatalog(new XMLCatalog());

    // Assert that nothing has changed
    EntityResolver entityResolver = xmlProperty.getEntityResolver();
    assertTrue(entityResolver instanceof XMLCatalog);
    Path catalogPath = ((XMLCatalog) entityResolver).getCatalogPath();
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XmlProperty#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>When {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_whenXMLCatalog() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    // Act
    xmlProperty.addConfiguredXMLCatalog(new XMLCatalog());

    // Assert
    EntityResolver entityResolver = xmlProperty.getEntityResolver();
    assertTrue(entityResolver instanceof XMLCatalog);
    Path catalogPath = ((XMLCatalog) entityResolver).getCatalogPath();
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XmlProperty#getFile()}.
   * <ul>
   *   <li>Given {@link XmlProperty} (default constructor) SrcResource is {@link Resource#Resource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#getFile()}
   */
  @Test
  public void testGetFile_givenXmlPropertySrcResourceIsResource_thenReturnNull() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    xmlProperty.setSrcResource(new Resource());

    // Act and Assert
    assertNull(xmlProperty.getFile());
  }

  /**
   * Test {@link XmlProperty#getFile()}.
   * <ul>
   *   <li>Then return Name is {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#getFile()}
   */
  @Test
  public void testGetFile_thenReturnNameIsNullFile() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    xmlProperty.setSrcResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER));

    // Act
    File actualFile = xmlProperty.getFile();

    // Assert
    assertEquals("NULL_FILE", actualFile.getName());
    assertTrue(actualFile.isAbsolute());
  }

  /**
   * Test {@link XmlProperty#getResource()}.
   * <ul>
   *   <li>Given {@link XmlProperty} (default constructor) SrcResource is {@link Resource#Resource()}.</li>
   *   <li>Then return {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#getResource()}
   */
  @Test
  public void testGetResource_givenXmlPropertySrcResourceIsResource_thenReturnResource() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    Resource src = new Resource();
    xmlProperty.setSrcResource(src);

    // Act and Assert
    assertSame(src, xmlProperty.getResource());
  }

  /**
   * Test {@link XmlProperty#getResource()}.
   * <ul>
   *   <li>Then return {@link FileResource#FileResource(File)} with f is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#getResource()}
   */
  @Test
  public void testGetResource_thenReturnFileResourceWithFIsNull_file_placeholder() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    FileResource src = new FileResource(Copy.NULL_FILE_PLACEHOLDER);
    xmlProperty.setSrcResource(src);

    // Act and Assert
    assertSame(src, xmlProperty.getResource());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link XmlProperty#setCollapseAttributes(boolean)}
   *   <li>{@link XmlProperty#setDelimiter(String)}
   *   <li>{@link XmlProperty#setIncludeSemanticAttribute(boolean)}
   *   <li>{@link XmlProperty#setKeeproot(boolean)}
   *   <li>{@link XmlProperty#setRootDirectory(File)}
   *   <li>{@link XmlProperty#setSemanticAttributes(boolean)}
   *   <li>{@link XmlProperty#setValidate(boolean)}
   *   <li>{@link XmlProperty#getCollapseAttributes()}
   *   <li>{@link XmlProperty#getDelimiter()}
   *   <li>{@link XmlProperty#getEntityResolver()}
   *   <li>{@link XmlProperty#getIncludeSemanticAttribute()}
   *   <li>{@link XmlProperty#getKeeproot()}
   *   <li>{@link XmlProperty#getPrefix()}
   *   <li>{@link XmlProperty#getRootDirectory()}
   *   <li>{@link XmlProperty#getSemanticAttributes()}
   *   <li>{@link XmlProperty#getValidate()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();

    // Act
    xmlProperty.setCollapseAttributes(true);
    xmlProperty.setDelimiter("Delimiter");
    xmlProperty.setIncludeSemanticAttribute(true);
    xmlProperty.setKeeproot(true);
    File rootDirectory = Copy.NULL_FILE_PLACEHOLDER;
    xmlProperty.setRootDirectory(rootDirectory);
    xmlProperty.setSemanticAttributes(true);
    xmlProperty.setValidate(true);
    boolean actualCollapseAttributes = xmlProperty.getCollapseAttributes();
    String actualDelimiter = xmlProperty.getDelimiter();
    EntityResolver actualEntityResolver = xmlProperty.getEntityResolver();
    boolean actualIncludeSemanticAttribute = xmlProperty.getIncludeSemanticAttribute();
    boolean actualKeeproot = xmlProperty.getKeeproot();
    String actualPrefix = xmlProperty.getPrefix();
    File actualRootDirectory = xmlProperty.getRootDirectory();
    boolean actualSemanticAttributes = xmlProperty.getSemanticAttributes();

    // Assert
    assertTrue(actualEntityResolver instanceof XMLCatalog);
    assertEquals("", actualPrefix);
    assertEquals("Delimiter", actualDelimiter);
    assertTrue(actualCollapseAttributes);
    assertTrue(actualIncludeSemanticAttribute);
    assertTrue(actualKeeproot);
    assertTrue(actualSemanticAttributes);
    assertTrue(xmlProperty.getValidate());
    assertSame(rootDirectory, actualRootDirectory);
  }

  /**
   * Test {@link XmlProperty#getIncludeSementicAttribute()}.
   * <ul>
   *   <li>Given {@link XmlProperty} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#getIncludeSementicAttribute()}
   */
  @Test
  public void testGetIncludeSementicAttribute_givenXmlProperty_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new XmlProperty()).getIncludeSementicAttribute());
  }

  /**
   * Test {@link XmlProperty#getIncludeSementicAttribute()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlProperty#getIncludeSementicAttribute()}
   */
  @Test
  public void testGetIncludeSementicAttribute_thenReturnTrue() {
    // Arrange
    XmlProperty xmlProperty = new XmlProperty();
    xmlProperty.setIncludeSemanticAttribute(true);

    // Act and Assert
    assertTrue(xmlProperty.getIncludeSementicAttribute());
  }

  /**
   * Test {@link XmlProperty#supportsNonFileResources()}.
   * <p>
   * Method under test: {@link XmlProperty#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources() {
    // Arrange, Act and Assert
    assertTrue((new XmlProperty()).supportsNonFileResources());
  }

  /**
   * Test new {@link XmlProperty} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link XmlProperty}
   */
  @Test
  public void testNewXmlProperty() {
    // Arrange and Act
    XmlProperty actualXmlProperty = new XmlProperty();

    // Assert
    assertTrue(actualXmlProperty.getEntityResolver() instanceof XMLCatalog);
    assertEquals("", actualXmlProperty.getPrefix());
    assertEquals(",", actualXmlProperty.getDelimiter());
    assertNull(actualXmlProperty.getRootDirectory());
    assertNull(actualXmlProperty.getDescription());
    assertNull(actualXmlProperty.getTaskName());
    assertNull(actualXmlProperty.getTaskType());
    assertNull(actualXmlProperty.getProject());
    assertNull(actualXmlProperty.getOwningTarget());
    assertFalse(actualXmlProperty.getCollapseAttributes());
    assertFalse(actualXmlProperty.getIncludeSemanticAttribute());
    assertFalse(actualXmlProperty.getIncludeSementicAttribute());
    assertFalse(actualXmlProperty.getSemanticAttributes());
    assertFalse(actualXmlProperty.getValidate());
    assertTrue(actualXmlProperty.getKeeproot());
  }
}
