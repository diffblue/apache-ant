package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import java.util.Stack;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.util.ChainedMapper;
import org.apache.tools.ant.util.CompositeMapper;
import org.junit.Test;

public class MappedResourceDiffblueTest {
  /**
   * Test {@link MappedResource#getName()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link ChainedMapper} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#getName()}
   */
  @Test
  public void testGetName_givenMappedResourceWithRIsResourceAndMIsChainedMapper_thenReturnNull() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertNull((new MappedResource(r, new ChainedMapper())).getName());
  }

  /**
   * Test {@link MappedResource#getName()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CompositeMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#getName()}
   */
  @Test
  public void testGetName_givenMappedResourceWithRIsResourceAndMIsCompositeMapper() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertNull((new MappedResource(r, new CompositeMapper())).getName());
  }

  /**
   * Test {@link MappedResource#getName()}.
   * <ul>
   *   <li>Then return {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#getName()}
   */
  @Test
  public void testGetName_thenReturnFileAttributeIsNull() {
    // Arrange
    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertEquals("file attribute is null!", (new MappedResource(r, new FilterMapper())).getName());
  }

  /**
   * Test {@link MappedResource#as(Class)}.
   * <p>
   * Method under test: {@link MappedResource#as(Class)}
   */
  @Test
  public void testAs() throws BuildException {
    // Arrange
    Resource r = new Resource();

    MappedResource mappedResource = new MappedResource(r, new CutDirsMapper());
    Stack<Object> stack = new Stack<>();
    mappedResource.dieOnCircularReference(stack, new Project());
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertSame(r, mappedResource.as(clazz));
  }

  /**
   * Test {@link MappedResource#as(Class)}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link MappedResource#MappedResource(Resource, FileNameMapper)} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#as(Class)}
   */
  @Test
  public void testAs_givenMappedResourceWithRIsMappedResourceAndMIsCutDirsMapper() {
    // Arrange
    Resource r = new Resource();
    MappedResource r2 = new MappedResource(r, new CutDirsMapper());

    MappedResource mappedResource = new MappedResource(r2, new CutDirsMapper());
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertSame(r, mappedResource.as(clazz));
  }

  /**
   * Test {@link MappedResource#as(Class)}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#as(Class)}
   */
  @Test
  public void testAs_givenMappedResourceWithRIsResourceAndMIsCutDirsMapper_thenReturnResource() {
    // Arrange
    Resource r = new Resource();
    MappedResource mappedResource = new MappedResource(r, new CutDirsMapper());
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertSame(r, mappedResource.as(clazz));
  }

  /**
   * Test {@link MappedResource#as(Class)}.
   * <ul>
   *   <li>When {@code FileProvider}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#as(Class)}
   */
  @Test
  public void testAs_whenOrgApacheToolsAntTypesResourcesFileProvider_thenReturnNull() {
    // Arrange
    Resource r = new Resource();
    MappedResource mappedResource = new MappedResource(r, new CutDirsMapper());
    Class<FileProvider> clazz = FileProvider.class;

    // Act and Assert
    assertNull(mappedResource.as(clazz));
  }

  /**
   * Test {@link MappedResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertNotEquals(new MappedResource(r, new CutDirsMapper()), "dirs must be set to a positive number");
  }

  /**
   * Test {@link MappedResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    FileResource r = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    MappedResource mappedResource = new MappedResource(r, new FilterMapper());
    Resource r2 = new Resource();

    // Act and Assert
    assertNotEquals(mappedResource, new MappedResource(r2, new FilterMapper()));
  }

  /**
   * Test {@link MappedResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual3() {
    // Arrange
    JavaConstantResource r = new JavaConstantResource();
    MappedResource mappedResource = new MappedResource(r, new FilterMapper());
    Resource r2 = new Resource();

    // Act and Assert
    assertNotEquals(mappedResource, new MappedResource(r2, new FilterMapper()));
  }

  /**
   * Test {@link MappedResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual4() {
    // Arrange
    Resource r = new Resource();
    MappedResource mappedResource = new MappedResource(r, new FilterMapper());
    FileResource r2 = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNotEquals(mappedResource, new MappedResource(r2, new FilterMapper()));
  }

  /**
   * Test {@link MappedResource#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertNotEquals(new MappedResource(r, new CutDirsMapper()), null);
  }

  /**
   * Test {@link MappedResource#equals(Object)}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then throw exception.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsSame_thenThrowException() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> (new MappedResource(r, new CutDirsMapper())).equals(new MappedResource(r, new CutDirsMapper())));
  }

  /**
   * Test {@link MappedResource#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertNotEquals(new MappedResource(r, new CutDirsMapper()), "Different type to MappedResource");
  }

  /**
   * Test {@link MappedResource#toString()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link ChainedMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#toString()}
   */
  @Test
  public void testToString_givenMappedResourceWithRIsResourceAndMIsChainedMapper() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertNull((new MappedResource(r, new ChainedMapper())).toString());
  }

  /**
   * Test {@link MappedResource#toString()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CompositeMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#toString()}
   */
  @Test
  public void testToString_givenMappedResourceWithRIsResourceAndMIsCompositeMapper() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertNull((new MappedResource(r, new CompositeMapper())).toString());
  }

  /**
   * Test {@link MappedResource#toString()}.
   * <ul>
   *   <li>Then return {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MappedResource#toString()}
   */
  @Test
  public void testToString_thenReturnFileAttributeIsNull() {
    // Arrange
    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertEquals("file attribute is null!", (new MappedResource(r, new FilterMapper())).toString());
  }
}
