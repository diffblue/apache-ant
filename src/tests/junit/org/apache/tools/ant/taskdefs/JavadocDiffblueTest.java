package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.Iterator;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Javadoc.AccessType;
import org.apache.tools.ant.taskdefs.Javadoc.DocletInfo;
import org.apache.tools.ant.taskdefs.Javadoc.DocletParam;
import org.apache.tools.ant.taskdefs.Javadoc.ExtensionInfo;
import org.apache.tools.ant.taskdefs.Javadoc.GroupArgument;
import org.apache.tools.ant.taskdefs.Javadoc.Html;
import org.apache.tools.ant.taskdefs.Javadoc.LinkArgument;
import org.apache.tools.ant.taskdefs.Javadoc.PackageName;
import org.apache.tools.ant.taskdefs.Javadoc.ResourceCollectionContainer;
import org.apache.tools.ant.taskdefs.Javadoc.SourceFile;
import org.apache.tools.ant.taskdefs.Javadoc.TagArgument;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.ResourceCollection;
import org.junit.Test;

public class JavadocDiffblueTest {
  /**
   * Test AccessType {@link AccessType#getValues()}.
   * <p>
   * Method under test: {@link AccessType#getValues()}
   */
  @Test
  public void testAccessTypeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"protected", "public", "package", "private"}, (new AccessType()).getValues());
  }

  /**
   * Test AccessType new {@link AccessType} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AccessType}
   */
  @Test
  public void testAccessTypeNewAccessType() {
    // Arrange and Act
    AccessType actualAccessType = new AccessType();

    // Assert
    assertNull(actualAccessType.getValue());
    assertEquals(-1, actualAccessType.getIndex());
  }

  /**
   * Test DocletInfo {@link DocletInfo#createParam()}.
   * <p>
   * Method under test: {@link DocletInfo#createParam()}
   */
  @Test
  public void testDocletInfoCreateParam() {
    // Arrange and Act
    DocletParam actualCreateParamResult = ((new Javadoc()).new DocletInfo()).createParam();

    // Assert
    assertNull(actualCreateParamResult.getName());
    assertNull(actualCreateParamResult.getValue());
  }

  /**
   * Test DocletInfo {@link DocletInfo#DocletInfo(Javadoc)}.
   * <p>
   * Method under test: {@link DocletInfo#DocletInfo(Javadoc)}
   */
  @Test
  public void testDocletInfoNewDocletInfo() {
    // Arrange and Act
    DocletInfo actualDocletInfo = (new Javadoc()).new DocletInfo();

    // Assert
    Location location = actualDocletInfo.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDocletInfo.getDescription());
    assertNull(actualDocletInfo.getName());
    assertNull(actualDocletInfo.getProject());
    assertNull(actualDocletInfo.getPath());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test DocletParam getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DocletParam#DocletParam(Javadoc)}
   *   <li>{@link DocletParam#setName(String)}
   *   <li>{@link DocletParam#setValue(String)}
   *   <li>{@link DocletParam#getName()}
   *   <li>{@link DocletParam#getValue()}
   * </ul>
   */
  @Test
  public void testDocletParamGettersAndSetters() {
    // Arrange and Act
    DocletParam actualDocletParam = (new Javadoc()).new DocletParam();
    actualDocletParam.setName(Manifest.ATTRIBUTE_NAME);
    actualDocletParam.setValue("42");
    String actualName = actualDocletParam.getName();

    // Assert
    assertEquals("42", actualDocletParam.getValue());
    assertEquals(Manifest.ATTRIBUTE_NAME, actualName);
  }

  /**
   * Test ExtensionInfo {@link ExtensionInfo#createPath()}.
   * <ul>
   *   <li>Then {@link ExtensionInfo} (default constructor) Path Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionInfo#createPath()}
   */
  @Test
  public void testExtensionInfoCreatePath_thenExtensionInfoPathDescriptionIsNull() {
    // Arrange
    ExtensionInfo extensionInfo = new ExtensionInfo();

    // Act
    Path actualCreatePathResult = extensionInfo.createPath();

    // Assert
    Path path = extensionInfo.getPath();
    assertNull(path.getDescription());
    assertNull(actualCreatePathResult.getProject());
    assertNull(path.getProject());
    assertNull(path.getRefid());
    assertEquals(0, path.size());
    assertFalse(path.isReference());
    assertTrue(path.isEmpty());
  }

  /**
   * Test ExtensionInfo {@link ExtensionInfo#createPath()}.
   * <ul>
   *   <li>Then {@link ExtensionInfo} (default constructor) Path is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionInfo#createPath()}
   */
  @Test
  public void testExtensionInfoCreatePath_thenExtensionInfoPathIsSystemBootClasspath() {
    // Arrange
    ExtensionInfo extensionInfo = new ExtensionInfo();
    extensionInfo.setPath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedPath = extensionInfo.createPath().systemBootClasspath;
    assertSame(expectedPath, extensionInfo.getPath());
  }

  /**
   * Test ExtensionInfo {@link ExtensionInfo#createPath()}.
   * <ul>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionInfo#createPath()}
   */
  @Test
  public void testExtensionInfoCreatePath_thenReturnProjectIsProject() {
    // Arrange
    ExtensionInfo extensionInfo = new ExtensionInfo();
    Project project = new Project();
    extensionInfo.setProject(project);

    // Act and Assert
    assertSame(project, extensionInfo.createPath().getProject());
    assertSame(project, extensionInfo.getPath().getProject());
  }

  /**
   * Test ExtensionInfo getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ExtensionInfo#setName(String)}
   *   <li>{@link ExtensionInfo#getName()}
   *   <li>{@link ExtensionInfo#getPath()}
   * </ul>
   */
  @Test
  public void testExtensionInfoGettersAndSetters() {
    // Arrange
    ExtensionInfo extensionInfo = new ExtensionInfo();

    // Act
    extensionInfo.setName(Manifest.ATTRIBUTE_NAME);
    String actualName = extensionInfo.getName();

    // Assert
    assertNull(extensionInfo.getPath());
    assertEquals(Manifest.ATTRIBUTE_NAME, actualName);
  }

  /**
   * Test ExtensionInfo new {@link ExtensionInfo} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ExtensionInfo}
   */
  @Test
  public void testExtensionInfoNewExtensionInfo() {
    // Arrange and Act
    ExtensionInfo actualExtensionInfo = new ExtensionInfo();

    // Assert
    Location location = actualExtensionInfo.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExtensionInfo.getDescription());
    assertNull(actualExtensionInfo.getName());
    assertNull(actualExtensionInfo.getProject());
    assertNull(actualExtensionInfo.getPath());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test ExtensionInfo {@link ExtensionInfo#setPath(Path)}.
   * <p>
   * Method under test: {@link ExtensionInfo#setPath(Path)}
   */
  @Test
  public void testExtensionInfoSetPath() {
    // Arrange
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    ExtensionInfo extensionInfo = new ExtensionInfo();
    extensionInfo.setPath(path);
    Path path2 = new Path(null);

    // Act
    extensionInfo.setPath(path2);

    // Assert that nothing has changed
    Path expectedPath = path2.systemBootClasspath;
    assertSame(expectedPath, extensionInfo.getPath());
  }

  /**
   * Test ExtensionInfo {@link ExtensionInfo#setPathRef(Reference)}.
   * <ul>
   *   <li>Then {@link ExtensionInfo} (default constructor) Path Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionInfo#setPathRef(Reference)}
   */
  @Test
  public void testExtensionInfoSetPathRef_thenExtensionInfoPathProjectIsNull() {
    // Arrange
    ExtensionInfo extensionInfo = new ExtensionInfo();

    // Act
    extensionInfo.setPathRef(new Reference("42"));

    // Assert
    Path path = extensionInfo.getPath();
    assertNull(path.getDescription());
    assertNull(path.getProject());
    assertNull(path.getRefid());
    assertFalse(path.isReference());
  }

  /**
   * Test ExtensionInfo {@link ExtensionInfo#setPathRef(Reference)}.
   * <ul>
   *   <li>Then {@link ExtensionInfo} (default constructor) Path Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionInfo#setPathRef(Reference)}
   */
  @Test
  public void testExtensionInfoSetPathRef_thenExtensionInfoPathProjectIsProject() {
    // Arrange
    ExtensionInfo extensionInfo = new ExtensionInfo();
    Project project = new Project();
    extensionInfo.setProject(project);

    // Act
    extensionInfo.setPathRef(new Reference("42"));

    // Assert
    Path path = extensionInfo.getPath();
    assertNull(path.getDescription());
    assertNull(path.getRefid());
    assertFalse(path.isReference());
    assertSame(project, path.getProject());
  }

  /**
   * Test GroupArgument {@link GroupArgument#addPackage(PackageName)}.
   * <p>
   * Method under test: {@link GroupArgument#addPackage(PackageName)}
   */
  @Test
  public void testGroupArgumentAddPackage() {
    // Arrange
    GroupArgument groupArgument = (new Javadoc()).new GroupArgument();

    PackageName pn = new PackageName();
    pn.setName(Manifest.ATTRIBUTE_NAME);

    // Act
    groupArgument.addPackage(pn);

    // Assert
    assertEquals(Manifest.ATTRIBUTE_NAME, groupArgument.getPackages());
  }

  /**
   * Test GroupArgument {@link GroupArgument#getPackages()}.
   * <p>
   * Method under test: {@link GroupArgument#getPackages()}
   */
  @Test
  public void testGroupArgumentGetPackages() {
    // Arrange, Act and Assert
    assertEquals("", ((new Javadoc()).new GroupArgument()).getPackages());
  }

  /**
   * Test GroupArgument {@link GroupArgument#getTitle()}.
   * <ul>
   *   <li>Given {@link GroupArgument#GroupArgument(Javadoc)} with this$0 is {@link Javadoc} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GroupArgument#getTitle()}
   */
  @Test
  public void testGroupArgumentGetTitle_givenGroupArgumentWithThis$0IsJavadoc_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(((new Javadoc()).new GroupArgument()).getTitle());
  }

  /**
   * Test GroupArgument {@link GroupArgument#getTitle()}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link GroupArgument#getTitle()}
   */
  @Test
  public void testGroupArgumentGetTitle_thenReturnEmptyString() {
    // Arrange
    GroupArgument groupArgument = (new Javadoc()).new GroupArgument();
    groupArgument.addTitle(new Html());

    // Act and Assert
    assertEquals("", groupArgument.getTitle());
  }

  /**
   * Test GroupArgument getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link GroupArgument#GroupArgument(Javadoc)}
   *   <li>{@link GroupArgument#addTitle(Html)}
   * </ul>
   */
  @Test
  public void testGroupArgumentGettersAndSetters() {
    // Arrange and Act
    GroupArgument actualGroupArgument = (new Javadoc()).new GroupArgument();
    actualGroupArgument.addTitle(new Html());

    // Assert
    assertEquals("", actualGroupArgument.getPackages());
    assertEquals("", actualGroupArgument.getTitle());
  }

  /**
   * Test GroupArgument {@link GroupArgument#setPackages(String)}.
   * <p>
   * Method under test: {@link GroupArgument#setPackages(String)}
   */
  @Test
  public void testGroupArgumentSetPackages() {
    // Arrange
    GroupArgument groupArgument = (new Javadoc()).new GroupArgument();

    // Act
    groupArgument.setPackages("Src");

    // Assert
    assertEquals("Src", groupArgument.getPackages());
  }

  /**
   * Test GroupArgument {@link GroupArgument#setTitle(String)}.
   * <p>
   * Method under test: {@link GroupArgument#setTitle(String)}
   */
  @Test
  public void testGroupArgumentSetTitle() {
    // Arrange
    GroupArgument groupArgument = (new Javadoc()).new GroupArgument();

    // Act
    groupArgument.setTitle("Src");

    // Assert
    assertEquals("Src", groupArgument.getTitle());
  }

  /**
   * Test Html {@link Html#addText(String)}.
   * <p>
   * Method under test: {@link Html#addText(String)}
   */
  @Test
  public void testHtmlAddText() {
    // Arrange
    Html html = new Html();

    // Act
    html.addText("foo");

    // Assert
    assertEquals("foo", html.getText());
  }

  /**
   * Test Html {@link Html#getText()}.
   * <p>
   * Method under test: {@link Html#getText()}
   */
  @Test
  public void testHtmlGetText() {
    // Arrange, Act and Assert
    assertEquals("", (new Html()).getText());
  }

  /**
   * Test Html new {@link Html} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Html}
   */
  @Test
  public void testHtmlNewHtml() {
    // Arrange, Act and Assert
    assertEquals("", (new Html()).getText());
  }

  /**
   * Test LinkArgument getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LinkArgument#LinkArgument(Javadoc)}
   *   <li>{@link LinkArgument#setHref(String)}
   *   <li>{@link LinkArgument#setOffline(boolean)}
   *   <li>{@link LinkArgument#setPackagelistLoc(File)}
   *   <li>{@link LinkArgument#setPackagelistURL(URL)}
   *   <li>{@link LinkArgument#setResolveLink(boolean)}
   *   <li>{@link LinkArgument#getHref()}
   *   <li>{@link LinkArgument#getPackagelistLoc()}
   *   <li>{@link LinkArgument#getPackagelistURL()}
   *   <li>{@link LinkArgument#isLinkOffline()}
   * </ul>
   */
  @Test
  public void testLinkArgumentGettersAndSetters() throws MalformedURLException {
    // Arrange and Act
    LinkArgument actualLinkArgument = (new Javadoc()).new LinkArgument();
    actualLinkArgument.setHref("Hr");
    actualLinkArgument.setOffline(true);
    File src = Copy.NULL_FILE_PLACEHOLDER;
    actualLinkArgument.setPackagelistLoc(src);
    URL src2 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL();
    actualLinkArgument.setPackagelistURL(src2);
    actualLinkArgument.setResolveLink(true);
    String actualHref = actualLinkArgument.getHref();
    File actualPackagelistLoc = actualLinkArgument.getPackagelistLoc();
    URL actualPackagelistURL = actualLinkArgument.getPackagelistURL();

    // Assert
    assertEquals("Hr", actualHref);
    assertTrue(actualLinkArgument.isLinkOffline());
    String expectedToStringResult = String.join("", "file:",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString().concat(File.separator));
    assertEquals(expectedToStringResult, actualPackagelistURL.toString());
    assertSame(src2, actualPackagelistURL);
    assertSame(src, actualPackagelistLoc);
  }

  /**
   * Test LinkArgument {@link LinkArgument#shouldResolveLink()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkArgument#shouldResolveLink()}
   */
  @Test
  public void testLinkArgumentShouldResolveLink_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(((new Javadoc()).new LinkArgument()).shouldResolveLink());
  }

  /**
   * Test LinkArgument {@link LinkArgument#shouldResolveLink()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkArgument#shouldResolveLink()}
   */
  @Test
  public void testLinkArgumentShouldResolveLink_thenReturnTrue() {
    // Arrange
    LinkArgument linkArgument = (new Javadoc()).new LinkArgument();
    linkArgument.setResolveLink(true);

    // Act and Assert
    assertTrue(linkArgument.shouldResolveLink());
  }

  /**
   * Test new {@link Javadoc} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Javadoc}
   */
  @Test
  public void testNewJavadoc() {
    // Arrange and Act
    Javadoc actualJavadoc = new Javadoc();

    // Assert
    Location location = actualJavadoc.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJavadoc.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJavadoc.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJavadoc.getTaskName());
    assertNull(actualJavadoc.getTaskType());
    assertNull(actualJavadoc.getProject());
    assertNull(actualJavadoc.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJavadoc, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test PackageName getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link PackageName}
   *   <li>{@link PackageName#toString()}
   *   <li>{@link PackageName#getName()}
   * </ul>
   */
  @Test
  public void testPackageNameGettersAndSetters() {
    // Arrange and Act
    PackageName actualPackageName = new PackageName();
    String actualToStringResult = actualPackageName.toString();

    // Assert
    assertNull(actualPackageName.getName());
    assertNull(actualToStringResult);
  }

  /**
   * Test PackageName {@link PackageName#setName(String)}.
   * <p>
   * Method under test: {@link PackageName#setName(String)}
   */
  @Test
  public void testPackageNameSetName() {
    // Arrange
    PackageName packageName = new PackageName();

    // Act
    packageName.setName(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertEquals(Manifest.ATTRIBUTE_NAME, packageName.getName());
  }

  /**
   * Test ResourceCollectionContainer {@link ResourceCollectionContainer#add(ResourceCollection)}.
   * <p>
   * Method under test: {@link ResourceCollectionContainer#add(ResourceCollection)}
   */
  @Test
  public void testResourceCollectionContainerAdd() {
    // Arrange
    ResourceCollectionContainer resourceCollectionContainer = (new Javadoc()).new ResourceCollectionContainer();
    Path rc = Path.systemBootClasspath;

    // Act
    resourceCollectionContainer.add(rc);

    // Assert
    Path expectedNextResult = rc.systemBootClasspath;
    Iterator<ResourceCollection> iteratorResult = resourceCollectionContainer.iterator();
    ResourceCollection actualNextResult = iteratorResult.next();
    assertFalse(iteratorResult.hasNext());
    assertSame(expectedNextResult, actualNextResult);
  }

  /**
   * Test ResourceCollectionContainer {@link ResourceCollectionContainer#iterator()}.
   * <p>
   * Method under test: {@link ResourceCollectionContainer#iterator()}
   */
  @Test
  public void testResourceCollectionContainerIterator() {
    // Arrange, Act and Assert
    assertFalse(((new Javadoc()).new ResourceCollectionContainer()).iterator().hasNext());
  }

  /**
   * Test ResourceCollectionContainer {@link ResourceCollectionContainer#ResourceCollectionContainer(Javadoc)}.
   * <p>
   * Method under test: {@link ResourceCollectionContainer#ResourceCollectionContainer(Javadoc)}
   */
  @Test
  public void testResourceCollectionContainerNewResourceCollectionContainer() {
    // Arrange, Act and Assert
    assertFalse(((new Javadoc()).new ResourceCollectionContainer()).iterator().hasNext());
  }

  /**
   * Test SourceFile getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SourceFile#SourceFile()}
   *   <li>{@link SourceFile#setFile(File)}
   *   <li>{@link SourceFile#getFile()}
   * </ul>
   */
  @Test
  public void testSourceFileGettersAndSetters() {
    // Arrange and Act
    SourceFile actualSourceFile = new SourceFile();
    File file = Copy.NULL_FILE_PLACEHOLDER;
    actualSourceFile.setFile(file);

    // Assert
    assertSame(file, actualSourceFile.getFile());
  }

  /**
   * Test SourceFile getters and setters.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SourceFile#SourceFile(File)}
   *   <li>{@link SourceFile#setFile(File)}
   *   <li>{@link SourceFile#getFile()}
   * </ul>
   */
  @Test
  public void testSourceFileGettersAndSetters_whenNull_file_placeholder() {
    // Arrange and Act
    SourceFile actualSourceFile = new SourceFile(Copy.NULL_FILE_PLACEHOLDER);
    File file = Copy.NULL_FILE_PLACEHOLDER;
    actualSourceFile.setFile(file);

    // Assert
    assertSame(file, actualSourceFile.getFile());
  }

  /**
   * Test TagArgument {@link TagArgument#getParameter()}.
   * <ul>
   *   <li>Given {@link TagArgument#TagArgument(Javadoc)} with this$0 is {@link Javadoc} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TagArgument#getParameter()}
   */
  @Test
  public void testTagArgumentGetParameter_givenTagArgumentWithThis$0IsJavadoc() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Javadoc()).new TagArgument()).getParameter());
  }

  /**
   * Test TagArgument {@link TagArgument#getParameter()}.
   * <ul>
   *   <li>Given {@link TagArgument#TagArgument(Javadoc)} with this$0 is {@link Javadoc} (default constructor) Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TagArgument#getParameter()}
   */
  @Test
  public void testTagArgumentGetParameter_givenTagArgumentWithThis$0IsJavadocNameIsEmptyString() throws BuildException {
    // Arrange
    TagArgument tagArgument = (new Javadoc()).new TagArgument();
    tagArgument.setName("");

    // Act and Assert
    assertThrows(BuildException.class, () -> tagArgument.getParameter());
  }

  /**
   * Test TagArgument {@link TagArgument#getParameter()}.
   * <ul>
   *   <li>Then return {@code No name specified for custom tag.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TagArgument#getParameter()}
   */
  @Test
  public void testTagArgumentGetParameter_thenReturnNoNameSpecifiedForCustomTag() throws BuildException {
    // Arrange
    TagArgument tagArgument = (new Javadoc()).new TagArgument();
    tagArgument.setName("No name specified for custom tag.");

    // Act and Assert
    assertEquals("No name specified for custom tag.", tagArgument.getParameter());
  }

  /**
   * Test TagArgument {@link TagArgument#TagArgument(Javadoc)}.
   * <p>
   * Method under test: {@link TagArgument#TagArgument(Javadoc)}
   */
  @Test
  public void testTagArgumentNewTagArgument() {
    // Arrange and Act
    TagArgument actualTagArgument = (new Javadoc()).new TagArgument();

    // Assert
    assertNull(actualTagArgument.getDir());
    Location location = actualTagArgument.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTagArgument.getDescription());
    assertNull(actualTagArgument.getProject());
    assertNull(actualTagArgument.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(5, actualTagArgument.getMaxLevelsOfSymlinks());
    assertFalse(actualTagArgument.isReference());
    assertTrue(actualTagArgument.getDefaultexcludes());
    assertTrue(actualTagArgument.getErrorOnMissingDir());
    assertTrue(actualTagArgument.isFilesystemOnly());
  }

  /**
   * Test TagArgument {@link TagArgument#setScope(String)}.
   * <ul>
   *   <li>When {@code ,}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TagArgument#setScope(String)}
   */
  @Test
  public void testTagArgumentSetScope_whenComma_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Javadoc()).new TagArgument()).setScope(","));
  }

  /**
   * Test TagArgument {@link TagArgument#setScope(String)}.
   * <ul>
   *   <li>When {@code Verbose Scope}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TagArgument#setScope(String)}
   */
  @Test
  public void testTagArgumentSetScope_whenVerboseScope_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Javadoc()).new TagArgument()).setScope("Verbose Scope"));
  }
}
